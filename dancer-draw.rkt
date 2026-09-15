#lang racket/base

;; Shared dancer figure: danceart's look (yellow/purple body, thick round green
;; LEFT / blue RIGHT arms) drawn to fill an arbitrary box, plus the clock-position
;; math and facing helpers.  Used by both the standalone dance snip and the dance
;; lane in the score snip.
;;
;; Sides follow the dancer's anatomy, matching danceart's make-dancer:
;;  - facing towards: the dancer's LEFT arm (green) is on the viewer's RIGHT
;;  - facing away:    mirrored -- green on the left, blue on the right
;;  - profile (left/right): both arms come from the centre, one drawn BEHIND the
;;    body and one IN FRONT, as reads spatially for a side view.

(require racket/class racket/draw racket/math)

(provide clock->vec vec->clock
         FACINGS facing->index index->facing
         draw-dancer dancer-arm-target)

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

;; --- geometry ------------------------------------------------------------
;; returns (values cx shoulder-y shoulder-dx arm-len arm-w body-w body-h body-cy)
(define (dancer-geom bx by bw bh)
  (define cx (+ bx (/ bw 2)))
  (define body-w (* bw 0.52))
  (define body-h (* bh 0.54))               ; a bit shorter
  (define body-cy (+ by (* bh 0.46)))
  (define sdx (* bw 0.22))
  (define arm-w (max 3 (round (* bh 0.07))))
  ;; the arm must poke past the body from ANY shoulder in ANY direction, so it is
  ;; never fully hidden: clear the body's vertical half-height, and its horizontal
  ;; half-width plus the shoulder offset (an inward arm crosses to the far edge),
  ;; with room for the round cap.  Then take the longer of that and a base length.
  (define reach (+ (max (/ body-h 2) (+ (/ body-w 2) sdx)) arm-w 3))
  (define arm-len (max (* bw 0.6) reach))
  (values cx body-cy sdx arm-len arm-w body-w body-h body-cy))

;; shoulder x of each arm given the facing: (values left-arm-x right-arm-x).
;; Front views put the dancer's left arm (green) on the viewer's right and the
;; right arm (blue) on the left (matching danceart, for both towards and away --
;; away differs only by colour and the arms hanging BEHIND the body).  A profile
;; centres both arms.
(define (arm-shoulder-xs facing cx sdx)
  (case facing
    [(towards away) (values (+ cx sdx) (- cx sdx))]   ; green(l) right, blue(r) left
    [else           (values cx cx)]))                 ; profile: centred

;; --- drawing -------------------------------------------------------------
(define (draw-dancer dc bx by bw bh l r facing)
  (define-values (cx sy sdx arm-len arm-w body-w body-h body-cy)
    (dancer-geom bx by bw bh))
  (define-values (lsx rsx) (arm-shoulder-xs facing cx sdx))
  (define (body!)
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
       (send dc draw-rectangle (if front-left? (+ bxx cw) bxx) byy cw body-h)]))
  (define (arm sx p color)
    (define v (clock->vec p))
    (send dc set-pen (make-pen #:color color #:width arm-w #:cap 'round #:join 'round))
    (send dc draw-line sx sy (+ sx (* arm-len (car v))) (+ sy (* arm-len (cdr v)))))
  (case facing
    [(towards)              ; facing us: arms in FRONT of the body
     (body!)
     (arm lsx l GREEN)      ; left arm, green (on the right)
     (arm rsx r BLUE)]      ; right arm, blue (on the left)
    [(away)                 ; facing away: same sides, arms BEHIND the body
     (arm lsx l GREEN)
     (arm rsx r BLUE)
     (body!)]
    [(left)                 ; profile: near arm in front, far arm behind
     (arm cx r BLUE)        ; right arm is the far side -> behind
     (body!)
     (arm cx l GREEN)]      ; left arm near -> in front
    [(right)
     (arm cx l GREEN)       ; left arm far -> behind
     (body!)
     (arm cx r BLUE)]))     ; right arm near -> in front

;; which arm a click targets and its new clock hour: (values 'l-or-'r hour).
;; Front-facing: the arm on the clicked side; profile: left half = l, right = r.
(define (dancer-arm-target bx by bw bh facing ex ey)
  (define-values (cx sy sdx arm-len arm-w body-w body-h body-cy)
    (dancer-geom bx by bw bh))
  (define-values (lsx rsx) (arm-shoulder-xs facing cx sdx))
  (define-values (which sx)
    (cond
      [(= lsx rsx) (if (< ex cx) (values 'l cx) (values 'r cx))]
      [(<= (abs (- ex lsx)) (abs (- ex rsx))) (values 'l lsx)]
      [else (values 'r rsx)]))
  (values which (vec->clock (- ex sx) (- ey sy))))
