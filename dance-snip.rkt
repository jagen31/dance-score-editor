#lang racket/base

;; An embedded DrRacket editor for one dance pose (a tonart arm-diagram).
;; Click the left/right half of the figure to point that arm at the clock
;; position of your click (12 = up, 3 = right, 6 = down, 9 = left); click the
;; label strip at the bottom to cycle which way the dancer faces.  The snip
;; persists in the .rkt file and emits `arm-diagram L R` + `facing DIR`.

(require racket/class
         racket/gui/base
         racket/snip
         racket/math
         racket/port
         shrubbery/parse    ; parse the emitted art code into a Rhombus term
         "art-common.rkt")  ; art->block: wrap the forms for read-special

(provide dance-snip% dance-snip-class snip-class)

;; --- geometry ------------------------------------------------------------
(define W 200)
(define H 230)
(define CX (quotient W 2))
(define SHOULDER-Y 88)               ; mid-body, so arms sit at the sides
(define SHOULDER-DX 33)              ; out near the body's edges
(define ARM-LEN 56)
(define ARM-W 8)                     ; thick, round-capped arms like the figures
(define BODY-W 78)
(define BODY-H 120)
(define BODY-CY 92)                  ; body-ellipse centre y
(define STRIP-Y (- H 26))           ; the facing-label strip

;; the dancer's palette (danceart: yellow body, purple back, green/blue arms)
(define YELLOW (make-object color% 250 224 0))
(define PURPLE (make-object color% 130 0 200))
(define GREEN  (make-object color% 0 170 0))
(define BLUE   (make-object color% 0 60 220))

(define FACINGS (vector 'towards 'right 'away 'left))
(define (facing->index f) (or (for/first ([g (in-vector FACINGS)] [i (in-naturals)] #:when (eq? f g)) i) 0))

;; clock position (1..12) -> unit vector (dx . dy), 12 = up
(define (clock->vec p)
  (define th (degrees->radians (* p 30)))
  (cons (sin th) (- (cos th))))

;; a click offset (dx dy) from a shoulder -> nearest clock position 1..12
(define (vec->clock dx dy)
  (define th (radians->degrees (atan dx (- dy))))   ; 0 = up, 90 = right
  (define p (modulo (inexact->exact (round (/ th 30))) 12))
  (if (= p 0) 12 p))

;; --- the snip ------------------------------------------------------------
(define dance-snip%
  (class* snip% (readable-snip<%>)               ; readable => reads AS its art code
    (init-field [l 6] [r 6] [facing 'towards])   ; both arms down, facing out
    (super-new)
    (inherit get-admin set-snipclass set-flags get-flags)
    (set-snipclass dance-snip-class)
    (set-flags (cons 'handles-events (get-flags)))

    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (when w (set-box! w (exact->inexact W)))
      (when h (set-box! h (exact->inexact H)))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ;; one thick, round-capped arm from a shoulder toward clock position `p`
    (define (arm dc x y sx sy p color)
      (define v (clock->vec p))
      (send dc set-pen (make-pen #:color color #:width ARM-W #:cap 'round #:join 'round))
      (send dc draw-line (+ x sx) (+ y sy)
            (+ x sx (* ARM-LEN (car v))) (+ y sy (* ARM-LEN (cdr v)))))

    ;; the body -- a coloured shape whose form shows the facing (danceart's
    ;; make-body): a yellow blob facing out, purple facing away, a yellow/purple
    ;; profile for left/right.
    (define (draw-body dc x y)
      (send dc set-pen (make-pen #:style 'transparent))
      (define bx (+ x (- CX (quotient BODY-W 2))))
      (define by (+ y (- BODY-CY (quotient BODY-H 2))))
      (define (fill c) (send dc set-brush c 'solid))
      (case facing
        [(towards) (fill YELLOW) (send dc draw-ellipse bx by BODY-W BODY-H)]
        [(away)    (fill PURPLE) (send dc draw-ellipse bx by BODY-W BODY-H)]
        [else
         ;; profile: two side-by-side columns, yellow (front) + purple (back);
         ;; mirror for `right`
         (define cw (quotient BODY-W 2))
         (define front-left? (eq? facing 'left))
         (fill YELLOW)
         (send dc draw-rectangle (if front-left? bx (+ bx cw)) by cw BODY-H)
         (fill PURPLE)
         (send dc draw-rectangle (if front-left? (+ bx cw) bx) by cw BODY-H)]))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc set-smoothing 'aligned)
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y W H)
      ;; body first, then the arms over it
      (draw-body dc x y)
      (arm dc x y (- CX SHOULDER-DX) SHOULDER-Y l GREEN)   ; left arm, green
      (arm dc x y (+ CX SHOULDER-DX) SHOULDER-Y r BLUE)    ; right arm, blue
      ;; facing strip
      (send dc set-pen "black" 1 'solid)
      (send dc draw-line (+ x 0) (+ y STRIP-Y) (+ x W) (+ y STRIP-Y))
      (send dc set-text-foreground "black")
      (send dc draw-text (format "L=~a R=~a  facing: ~a  (click to edit)" l r facing)
            (+ x 8) (+ y STRIP-Y 5)))

    (define/override (on-event dc x y editorx editory evt)
      (when (send evt button-down? 'left)
        (define ex (- (send evt get-x) x))
        (define ey (- (send evt get-y) y))
        (cond
          [(>= ey STRIP-Y)
           ;; cycle facing
           (set! facing (vector-ref FACINGS (modulo (add1 (facing->index facing)) 4)))]
          [(< ex CX)
           (set! l (vec->clock (- ex (- CX SHOULDER-DX)) (- ey SHOULDER-Y)))]
          [else
           (set! r (vec->clock (- ex (+ CX SHOULDER-DX)) (- ey SHOULDER-Y)))])
        (define a (get-admin))
        (when a (send a needs-update this 0 0 W H))))

    (define/override (copy) (new dance-snip% [l l] [r r] [facing facing]))

    (define/override (write f)
      (send f put l) (send f put r) (send f put (facing->index facing)))

    ;; danceart (Art 4): the pose is `arm_diagram L R` under a `facing` coord
    (define/public (->art-string)
      (format "at [facing ~a]: arm_diagram ~a ~a\n" facing l r))

    ;; read AS code when the file is run.  A `read-special` result becomes ONE
    ;; term where the snip sits, so we wrap the pose in a single `at []:` block
    ;; (empty coords = identity); `parse-all` turns that string into shrubbery.
    ;; We give the parsed forms NO lexical context (`datum->syntax #f`): when the
    ;; enclosing `#lang rhombus` module is expanded it stamps its OWN scopes onto
    ;; them, so `at` / `facing` / `arm_diagram` bind to whatever that module
    ;; imported (danceart).  A borrowed context would shadow that and the facade
    ;; interpreter would reject the forms as "unknown art form".
    (define/public (read-special src line col pos)
      (datum->syntax
       #f
       (syntax->datum
        (parse-all (open-input-string (art->block (send this ->art-string)))
                   #:source src))))))

;; --- the snip class (persistence) ---------------------------------------
(define dance-snip-class%
  (class snip-class%
    (super-new)
    (define/override (read f)
      (define l (send f get-exact))
      (define r (send f get-exact))
      (define fi (send f get-exact))
      (new dance-snip% [l l] [r r] [facing (vector-ref FACINGS (modulo fi 4))]))))

(define dance-snip-class (new dance-snip-class%))
(send dance-snip-class set-classname "score-dance-editor:dance")
(send dance-snip-class set-version 1)
(send (get-the-snip-class-list) add dance-snip-class)

(define snip-class dance-snip-class)
