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
         shrubbery/parse)   ; parse the emitted art code into a Rhombus term

(provide dance-snip% dance-snip-class snip-class)

;; --- geometry ------------------------------------------------------------
(define W 200)
(define H 230)
(define CX (quotient W 2))
(define SHOULDER-Y 70)
(define SHOULDER-DX 22)
(define ARM-LEN 46)
(define STRIP-Y (- H 26))            ; the facing-label strip

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

    (define (arm dc x y sx sy p)
      (define v (clock->vec p))
      (send dc draw-line (+ x sx) (+ y sy)
            (+ x sx (* ARM-LEN (car v))) (+ y sy (* ARM-LEN (cdr v)))))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y W H)
      (send dc set-pen "black" 3 'solid)
      ;; head
      (send dc set-brush "white" 'solid)
      (send dc draw-ellipse (+ x CX -14) (+ y 22) 28 28)
      ;; body + legs
      (send dc draw-line (+ x CX) (+ y 50) (+ x CX) (+ y 130))
      (send dc draw-line (+ x CX) (+ y 130) (+ x CX -26) (+ y 178))
      (send dc draw-line (+ x CX) (+ y 130) (+ x CX 26) (+ y 178))
      ;; arms (left shoulder is on the dancer's left = screen right; but keep it
      ;; simple/mirror-free: `l` draws on the left of the snip, `r` on the right)
      (send dc set-pen "blue" 3 'solid)
      (arm dc x y (- CX SHOULDER-DX) SHOULDER-Y l)
      (send dc set-pen "red" 3 'solid)
      (arm dc x y (+ CX SHOULDER-DX) SHOULDER-Y r)
      ;; shoulders
      (send dc set-pen "black" 3 'solid)
      (send dc draw-line (+ x CX (- SHOULDER-DX)) (+ y SHOULDER-Y)
            (+ x CX SHOULDER-DX) (+ y SHOULDER-Y))
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

    ;; read AS code when the file is run: parse the art string into a Rhombus
    ;; term so the snip becomes `at [facing ...]: arm_diagram ...` in place.
    ;; Strip the parse's lexical context (datum->syntax #f ...) so the enclosing
    ;; module's own scope binds `at` / `facing` / `arm_diagram` -- otherwise the
    ;; parsed identifiers carry shrubbery's scopes and don't match facade's.
    (define/public (read-special src line col pos)
      (datum->syntax
       #f
       (syntax->datum
        (parse-all (open-input-string (send this ->art-string)) #:source src))))))

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
