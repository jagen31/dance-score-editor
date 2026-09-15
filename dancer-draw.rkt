#lang racket/base

;; Shared dancer figure: danceart's look (yellow/purple body, thick round green
;; left / blue right arms) drawn to fill an arbitrary box, plus the clock-position
;; math and facing helpers.  Used by both the standalone dance snip and the dance
;; lane in the score snip.

(require racket/class racket/draw racket/math)

(provide clock->vec vec->clock
         FACINGS facing->index index->facing
         dancer-geom dancer-shoulders draw-dancer)

;; --- palette -------------------------------------------------------------
(define YELLOW (make-object color% 250 224 0))
(define PURPLE (make-object color% 130 0 200))
(define GREEN  (make-object color% 0 170 0))
(define BLUE   (make-object color% 0 60 220))

;; --- facing --------------------------------------------------------------
(define FACINGS (vector 'towards 'right 'away 'left))
(define (facing->index f)
  (or (for/first ([g (in-vector FACINGS)] [i (in-naturals)] #:when (eq? f g)) i) 0))
(define (index->facing i) (vector-ref FACINGS (modulo i 4)))

;; --- clock positions -----------------------------------------------------
;; clock hour (1..12) -> unit vector (dx . dy); 12 = up, 3 = right
(define (clock->vec p)
  (define th (degrees->radians (* p 30)))
  (cons (sin th) (- (cos th))))
;; a click offset (dx dy) from a shoulder -> nearest clock hour 1..12
(define (vec->clock dx dy)
  (define th (radians->degrees (atan dx (- dy))))   ; 0 = up, 90 = right
  (define p (modulo (inexact->exact (round (/ th 30))) 12))
  (if (= p 0) 12 p))

;; --- geometry of a dancer filling box (bx by bw bh) ----------------------
;; returns (values cx shoulder-y shoulder-dx arm-len arm-w body-w body-h body-cy)
(define (dancer-geom bx by bw bh)
  (define cx (+ bx (/ bw 2)))
  (define body-w (* bw 0.52))
  (define body-h (* bh 0.66))
  (define body-cy (+ by (* bh 0.46)))
  (values cx body-cy (* bw 0.22) (* bw 0.38) (max 3 (round (* bh 0.07)))
          body-w body-h body-cy))

;; the left/right shoulder points, for mapping a click to an arm: (values lx ly rx ry)
(define (dancer-shoulders bx by bw bh)
  (define-values (cx sy sdx al aw bw2 bh2 bcy) (dancer-geom bx by bw bh))
  (values (- cx sdx) sy (+ cx sdx) sy))

;; draw a dancer (arms at clock hours l/r, facing symbol) filling the box
(define (draw-dancer dc bx by bw bh l r facing)
  (define-values (cx sy sdx arm-len arm-w body-w body-h body-cy)
    (dancer-geom bx by bw bh))
  (send dc set-pen (make-pen #:style 'transparent))
  (define bxx (- cx (/ body-w 2)))
  (define byy (- body-cy (/ body-h 2)))
  (case facing
    [(towards) (send dc set-brush YELLOW 'solid) (send dc draw-ellipse bxx byy body-w body-h)]
    [(away)    (send dc set-brush PURPLE 'solid) (send dc draw-ellipse bxx byy body-w body-h)]
    [else
     ;; profile: yellow front column + purple back column (mirror for `right`)
     (define cw (/ body-w 2))
     (define front-left? (eq? facing 'left))
     (send dc set-brush YELLOW 'solid)
     (send dc draw-rectangle (if front-left? bxx (+ bxx cw)) byy cw body-h)
     (send dc set-brush PURPLE 'solid)
     (send dc draw-rectangle (if front-left? (+ bxx cw) bxx) byy cw body-h)])
  (define (arm sx p color)
    (define v (clock->vec p))
    (send dc set-pen (make-pen #:color color #:width arm-w #:cap 'round #:join 'round))
    (send dc draw-line sx sy (+ sx (* arm-len (car v))) (+ sy (* arm-len (cdr v)))))
  (arm (- cx sdx) l GREEN)     ; left arm, green
  (arm (+ cx sdx) r BLUE))     ; right arm, blue
