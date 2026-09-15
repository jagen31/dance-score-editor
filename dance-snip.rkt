#lang racket/base

;; An embedded DrRacket editor for one dance pose (a tonart arm-diagram).
;; Press on an arm and DRAG to swing it around (12 = up, 3 = right, 6 = down,
;; 9 = left); click the label strip at the bottom to cycle which way the dancer
;; faces.  The snip persists in the .rkt file and emits `arm_diagram L R` under
;; a `facing`.
;;
;; (The score snip embeds the same figure in a lane above the staff; the shape
;; and clock/facing math live in dancer-draw.rkt so both stay identical.)

(require racket/class
         racket/gui/base
         racket/snip
         racket/port
         shrubbery/parse    ; parse the emitted art code into a Rhombus term
         "art-common.rkt"   ; art->block: wrap the forms for read-special
         "dancer-draw.rkt") ; the shared dancer figure + clock/facing helpers

(provide dance-snip% dance-snip-class snip-class)

;; --- geometry ------------------------------------------------------------
(define W 200)
(define H 230)
(define STRIP-Y (- H 26))           ; the facing-label strip
(define BX 8) (define BY 6)         ; the figure box within the snip
(define BW (- W 16)) (define BH (- STRIP-Y 12))
(define CX (+ BX (/ BW 2)))

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

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc set-smoothing 'aligned)
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y W H)
      (draw-dancer dc (+ x BX) (+ y BY) BW BH l r facing)
      ;; facing strip
      (send dc set-pen "black" 1 'solid)
      (send dc draw-line (+ x 0) (+ y STRIP-Y) (+ x W) (+ y STRIP-Y))
      (send dc set-text-foreground "black")
      (send dc draw-text (format "L=~a R=~a  facing: ~a  (drag an arm)" l r facing)
            (+ x 8) (+ y STRIP-Y 5)))

    (define arm-drag #f)   ; 'l or 'r while an arm is being dragged
    (define (refresh) (define a (get-admin)) (when a (send a needs-update this 0 0 W H)))

    (define/override (on-event dc x y editorx editory evt)
      (define ex (- (send evt get-x) x))
      (define ey (- (send evt get-y) y))
      (cond
        ;; drag the grabbed arm to follow the cursor
        [(and arm-drag (send evt dragging?))
         (define hour (dancer-arm-hour BX BY BW BH facing arm-drag ex ey))
         (if (eq? arm-drag 'l) (set! l hour) (set! r hour))
         (refresh)]
        [(and arm-drag (send evt button-up? 'left)) (set! arm-drag #f)]
        [(send evt button-down? 'left)
         (cond
           [(>= ey STRIP-Y) (set! facing (index->facing (add1 (facing->index facing)))) (refresh)]
           [else
            ;; grab the arm on the clicked side; drag to aim it
            (define-values (which hour) (dancer-arm-target BX BY BW BH facing ex ey))
            (if (eq? which 'l) (set! l hour) (set! r hour))
            (set! arm-drag which)
            (refresh)])]))

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
      (new dance-snip% [l l] [r r] [facing (index->facing fi)]))))

(define dance-snip-class (new dance-snip-class%))
(send dance-snip-class set-classname "score-dance-editor:dance")
(send dance-snip-class set-version 1)
(send (get-the-snip-class-list) add dance-snip-class)

(define snip-class dance-snip-class)
