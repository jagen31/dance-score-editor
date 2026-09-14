#lang racket/base

;; An embedded DrRacket editor for a short musical score.  Click cells on the
;; staff to toggle notes; the snip persists in the .rkt file and can emit the
;; equivalent tonart art forms (`->art-string`) for pasting into a program.
;;
;; Rows run top (high) to bottom (low), one diatonic step each; the five staff
;; lines are the treble staff (E4 G4 B4 D5 F5).  Columns are beats.

(require racket/class
         racket/gui/base
         racket/snip
         racket/math
         racket/list
         racket/port
         shrubbery/parse)

(provide score-snip% score-snip-class snip-class)

;; --- geometry ------------------------------------------------------------
(define COLS 16)
(define ROWS 17)                      ; rows 0..16, top..bottom
(define CELL-W 22)
(define CELL-H 9)
(define MARGIN 12)
(define W (+ (* 2 MARGIN) (* COLS CELL-W)))
(define H (+ (* 2 MARGIN) (* (sub1 ROWS) CELL-H)))
(define STAFF-ROWS '(4 6 8 10 12))    ; rows drawn as staff lines

;; row -> (values pitch-letter octave).  Row 12 is E4 (bottom staff line);
;; each row up is one diatonic step.
(define LETTERS (vector "c" "d" "e" "f" "g" "a" "b"))
(define (row->pitch r)
  (define d (- 14 r))
  (values (vector-ref LETTERS (modulo d 7))
          (+ 4 (inexact->exact (floor (/ d 7))))))

(define (sorted-keys notes)
  (sort (hash-keys notes)
        (lambda (a b) (or (< (car a) (car b))
                          (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))

;; --- the snip ------------------------------------------------------------
(define score-snip%
  (class* snip% (readable-snip<%>)
    (init-field [notes (make-hash)])   ; (cons col row) -> #t
    (super-new)
    (inherit get-admin set-snipclass set-flags get-flags)
    (set-snipclass score-snip-class)
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
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y W H)
      ;; faint beat gridlines
      (send dc set-pen (make-object color% 225 225 225) 1 'solid)
      (for ([c (in-range (add1 COLS))])
        (define cx (+ x MARGIN (* c CELL-W)))
        (send dc draw-line cx (+ y MARGIN) cx (+ y (- H MARGIN))))
      ;; staff lines
      (send dc set-pen "black" 1 'solid)
      (for ([r (in-list STAFF-ROWS)])
        (define ry (+ y MARGIN (* r CELL-H)))
        (send dc draw-line (+ x MARGIN) ry (+ x (- W MARGIN)) ry))
      ;; note heads
      (send dc set-brush "black" 'solid)
      (for ([k (in-list (sorted-keys notes))])
        (define nx (+ x MARGIN (* (car k) CELL-W) 2))
        (define ny (+ y MARGIN (* (cdr k) CELL-H) (- (quotient CELL-H 2))))
        (send dc draw-ellipse nx ny (- CELL-W 4) CELL-H)))

    ;; nearest (col . row) for a snip-local point, or #f if off the grid
    (define (cell-at ex ey)
      (define c (inexact->exact (floor (/ (- ex MARGIN) CELL-W))))
      (define r (inexact->exact (round (/ (- ey MARGIN) CELL-H))))
      (and (>= c 0) (< c COLS) (>= r 0) (< r ROWS) (cons c r)))

    (define/override (on-event dc x y editorx editory evt)
      (when (send evt button-down? 'left)
        (define cell (cell-at (- (send evt get-x) x) (- (send evt get-y) y)))
        (when cell
          (if (hash-ref notes cell #f)
              (hash-remove! notes cell)
              (hash-set! notes cell #t))
          (define a (get-admin))
          (when a (send a needs-update this 0 0 W H)))))

    (define/override (copy)
      (new score-snip% [notes (hash-copy notes)]))

    (define/override (write f)
      (define ks (sorted-keys notes))
      (send f put (length ks))
      (for ([k (in-list ks)]) (send f put (car k)) (send f put (cdr k))))

    ;; the score as tonart art forms (one note per toggled cell)
    (define/public (->art-string)
      (apply string-append
             (for/list ([k (in-list (sorted-keys notes))])
               (define-values (letter oct) (row->pitch (cdr k)))
               (format "at [interval ~a ~a]: note ~a 0 ~a\n"
                       (car k) (add1 (car k)) letter oct))))

    ;; read AS code when the file is run: the note forms as a Rhombus term
    ;; sequence (splices where the snip sits -- e.g. inside a `music:` block).
    ;; Strip the parse's lexical context so the enclosing module's scope binds
    ;; `at` / `interval` / `note` (see dance-snip for why).
    (define/public (read-special src line col pos)
      (datum->syntax
       #f
       (syntax->datum
        (parse-all (open-input-string (send this ->art-string)) #:source src))))))

;; --- the snip class (persistence) ---------------------------------------
(define score-snip-class%
  (class snip-class%
    (super-new)
    (define/override (read f)
      (define n (send f get-exact))
      (define notes (make-hash))
      (for ([_ (in-range n)])
        (define c (send f get-exact))
        (define r (send f get-exact))
        (hash-set! notes (cons c r) #t))
      (new score-snip% [notes notes]))))

(define score-snip-class (new score-snip-class%))
(send score-snip-class set-classname "score-dance-editor:score")
(send score-snip-class set-version 1)
(send (get-the-snip-class-list) add score-snip-class)

;; the name DrRacket looks up to auto-load this class when reading a file
(define snip-class score-snip-class)
