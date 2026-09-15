#lang racket/base

;; An embedded DrRacket editor for a score with dances.  Click cells on the
;; staff to toggle notes; click the lane ABOVE the staff to place dancers, which
;; snap to the 16th-note columns and hang over the score.  The grid extends
;; rightward without limit: click the `+` strip at the right edge to add more
;; columns (right-click it to trim empty ones).  The snip persists in the .rkt
;; file and emits the equivalent tonart art forms (`->art-string`).
;;
;; Notes: rows run top (high) to bottom (low), one diatonic step each; the five
;; staff lines are the treble staff (E4 G4 B4 D5 F5).  Columns are 16th notes.
;; Dancers: one per column, drawn in the lane; left-click a column to add one,
;; click near a placed dancer to aim an arm (left half = green/left arm, right
;; half = blue/right arm), click the strip under it to cycle facing, right-click
;; to remove.

(require racket/class
         racket/gui/base
         racket/snip
         racket/math
         racket/list
         racket/port
         racket/vector
         shrubbery/parse
         "art-common.rkt"     ; art->block: wrap the forms for read-special
         "dancer-draw.rkt")   ; the shared dancer figure + clock/facing helpers

(provide score-snip% score-snip-class snip-class)

;; --- geometry ------------------------------------------------------------
(define DEFAULT-COLS 16)
(define GROW 8)                       ; columns added/removed per `+`/`-` click
(define ROWS 17)                      ; rows 0..16, top..bottom
(define CELL-W 22)
(define CELL-H 9)
(define MARGIN 12)
(define ADD-W 30)                     ; the `+`/`-` strip at the right edge
(define DANCE-LANE-H 108)             ; room above the staff for dancers
(define DANCER-W 44)                  ; a dancer's drawn width (spans ~2 columns)
(define DANCER-BH 90)                 ; a dancer's figure height (strip below it)
(define GRID-TOP (+ MARGIN DANCE-LANE-H))    ; y where the note grid starts
(define H (+ GRID-TOP (* (sub1 ROWS) CELL-H) MARGIN))
(define STAFF-ROWS '(4 6 8 10 12))    ; rows drawn as staff lines

;; row -> (values pitch-letter octave).  Row 12 is E4 (bottom staff line).
(define LETTERS (vector "c" "d" "e" "f" "g" "a" "b"))
(define (row->pitch r)
  (define d (- 14 r))
  (values (vector-ref LETTERS (modulo d 7))
          (+ 4 (inexact->exact (floor (/ d 7))))))

(define (sorted-keys notes)
  (sort (hash-keys notes)
        (lambda (a b) (or (< (car a) (car b))
                          (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))

;; centre x of column c, and the dancer box for that column (snip-local)
(define (col-center c) (+ MARGIN (* c CELL-W) (/ CELL-W 2)))
(define (dbox-x c) (- (col-center c) (/ DANCER-W 2)))

(define LABEL-FONT (make-object font% 9 'default 'normal 'normal))
(define PLUS-FONT (make-object font% 18 'default 'normal 'bold))

;; --- the snip ------------------------------------------------------------
(define score-snip%
  (class* snip% (readable-snip<%>)
    (init-field [notes (make-hash)]       ; (cons col row) -> #t
                [dancers (make-hash)]     ; col -> (vector l r facing-symbol)
                [cols DEFAULT-COLS])      ; how many 16th-note columns are shown
    (super-new)
    (inherit get-admin set-snipclass set-flags get-flags)
    (set-snipclass score-snip-class)
    (set-flags (cons 'handles-events (get-flags)))

    (define (grid-right) (+ MARGIN (* cols CELL-W)))    ; x past the last column
    (define (width) (+ (grid-right) ADD-W))
    ;; the highest column any note/dancer uses (or -1 if empty)
    (define (max-used-col)
      (apply max -1 (append (map car (hash-keys notes)) (hash-keys dancers))))
    (define (min-cols) (max GROW (add1 (max-used-col))))

    (define (refresh)
      (define a (get-admin))
      (when a (send a needs-update this 0 0 (width) H)))
    (define (resized!)
      (define a (get-admin))
      (when a (send a resized this #t) (send a needs-update this 0 0 (width) H)))

    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (when w (set-box! w (exact->inexact (width))))
      (when h (set-box! h (exact->inexact H)))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y (width) H)
      ;; faint column gridlines, full height, so dancers visibly snap to 16ths
      (send dc set-pen (make-object color% 225 225 225) 1 'solid)
      (for ([c (in-range (add1 cols))])
        (define cx (+ x MARGIN (* c CELL-W)))
        (send dc draw-line cx (+ y MARGIN) cx (+ y (- H MARGIN))))
      ;; separator between the dance lane and the staff
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-line (+ x MARGIN) (+ y GRID-TOP -4) (+ x (grid-right)) (+ y GRID-TOP -4))
      ;; dancers in the lane
      (send dc set-font LABEL-FONT)
      (send dc set-text-foreground (make-object color% 120 120 120))
      (for ([col (in-list (sort (hash-keys dancers) <))])
        (define v (hash-ref dancers col))
        (draw-dancer dc (+ x (dbox-x col)) (+ y MARGIN) DANCER-W DANCER-BH
                     (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))
        (send dc draw-text (symbol->string (vector-ref v 2))
              (+ x (dbox-x col) 2) (+ y MARGIN DANCER-BH)))
      ;; staff lines
      (send dc set-pen "black" 1 'solid)
      (for ([r (in-list STAFF-ROWS)])
        (define ry (+ y GRID-TOP (* r CELL-H)))
        (send dc draw-line (+ x MARGIN) ry (+ x (grid-right)) ry))
      ;; note heads
      (send dc set-brush "black" 'solid)
      (for ([k (in-list (sorted-keys notes))])
        (define nx (+ x MARGIN (* (car k) CELL-W) 2))
        (define ny (+ y GRID-TOP (* (cdr k) CELL-H) (- (quotient CELL-H 2))))
        (send dc draw-ellipse nx ny (- CELL-W 4) CELL-H))
      ;; the `+` strip at the right edge (right-click trims): a light button
      (define ax (+ x (grid-right)))
      (send dc set-brush (make-object color% 240 240 240) 'solid)
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-rectangle (+ ax 3) (+ y GRID-TOP) (- ADD-W 6) (- H GRID-TOP MARGIN))
      (send dc set-font PLUS-FONT)
      (send dc set-text-foreground (make-object color% 120 120 120))
      (send dc draw-text "+" (+ ax 8) (+ y GRID-TOP 6)))

    ;; --- hit testing -------------------------------------------------------
    (define (in-add-zone? ex) (>= ex (grid-right)))
    ;; nearest (col . row) note cell for a snip-local point, or #f
    (define (cell-at ex ey)
      (define c (inexact->exact (floor (/ (- ex MARGIN) CELL-W))))
      (define r (inexact->exact (round (/ (- ey GRID-TOP) CELL-H))))
      (and (>= c 0) (< c cols) (>= r 0) (< r ROWS) (cons c r)))
    ;; the column for a lane click (for placing a new dancer), or #f
    (define (col-at ex)
      (define c (inexact->exact (floor (/ (- ex MARGIN) CELL-W))))
      (and (>= c 0) (< c cols) c))
    ;; the placed dancer nearest ex (within half a dancer width), or #f
    (define (dancer-hit ex)
      (for/fold ([best #f] [bd +inf.0] #:result best)
                ([col (in-list (hash-keys dancers))])
        (define d (abs (- ex (col-center col))))
        (if (and (<= d (/ DANCER-W 2)) (< d bd)) (values col d) (values best bd))))

    ;; set an arm of the dancer at `col` from a click (which arm + hour is decided
    ;; by the shared figure geometry, so it tracks the facing-aware sides)
    (define (edit-arm! col ex ey)
      (define v (hash-ref dancers col))
      (define-values (which hour)
        (dancer-arm-target (dbox-x col) MARGIN DANCER-W DANCER-BH (vector-ref v 2) ex ey))
      (if (eq? which 'l) (vector-set! v 0 hour) (vector-set! v 1 hour)))

    (define/override (on-event dc x y editorx editory evt)
      (define ex (- (send evt get-x) x))
      (define ey (- (send evt get-y) y))
      (cond
        [(send evt button-down? 'right)
         (cond
           ;; right-click the `+` strip: trim empty columns from the right
           [(in-add-zone? ex) (set! cols (max (min-cols) (- cols GROW))) (resized!)]
           ;; right-click a dancer: remove it
           [(< ey GRID-TOP)
            (define hit (dancer-hit ex))
            (when hit (hash-remove! dancers hit) (refresh))])]
        [(send evt button-down? 'left)
         (cond
           ;; the `+` strip: extend the grid rightward
           [(in-add-zone? ex) (set! cols (+ cols GROW)) (resized!)]
           ;; --- the dance lane ---
           [(< ey GRID-TOP)
            (define hit (dancer-hit ex))
            (cond
              ;; strip under a dancer: cycle its facing
              [(and hit (>= ey (+ MARGIN DANCER-BH)))
               (define v (hash-ref dancers hit))
               (vector-set! v 2 (index->facing (add1 (facing->index (vector-ref v 2)))))
               (refresh)]
              ;; near a placed dancer: aim an arm
              [hit (edit-arm! hit ex ey) (refresh)]
              ;; empty column: place a default dancer (arms down, facing out)
              [else
               (define c (col-at ex))
               (when (and c (not (hash-ref dancers c #f)))
                 (hash-set! dancers c (vector 6 6 'towards)) (refresh))])]
           ;; --- the staff: toggle a note ---
           [else
            (define cell (cell-at ex ey))
            (when cell
              (if (hash-ref notes cell #f) (hash-remove! notes cell) (hash-set! notes cell #t))
              (refresh))])]))

    (define/override (copy)
      (new score-snip% [notes (hash-copy notes)] [dancers (copy-dancers dancers)] [cols cols]))

    (define/override (write f)
      (define ks (sorted-keys notes))
      (send f put (length ks))
      (for ([k (in-list ks)]) (send f put (car k)) (send f put (cdr k)))
      (define ds (sort (hash-keys dancers) <))
      (send f put (length ds))
      (for ([col (in-list ds)])
        (define v (hash-ref dancers col))
        (send f put col) (send f put (vector-ref v 0)) (send f put (vector-ref v 1))
        (send f put (facing->index (vector-ref v 2)))))
    ;; NB: `cols` is intentionally NOT persisted -- adding a trailing field breaks
    ;; older saved snips (the reader overreads).  On load the grid is sized to fit
    ;; its content instead (see the class `read`), and the `+` extends it per session.

    ;; the score as tonart art forms: a note per toggled cell, a dancer per pose
    ;; (each under a `facing` and placed at its column's 16th-note interval)
    (define/public (->art-string)
      (string-append
       (apply string-append
              (for/list ([k (in-list (sorted-keys notes))])
                (define-values (letter oct) (row->pitch (cdr k)))
                (format "at [interval ~a ~a]: note ~a 0 ~a\n"
                        (car k) (add1 (car k)) letter oct)))
       (apply string-append
              (for/list ([col (in-list (sort (hash-keys dancers) <))])
                (define v (hash-ref dancers col))
                (format "at [facing ~a]: at [interval ~a ~a]: arm_diagram ~a ~a\n"
                        (vector-ref v 2) col (add1 col)
                        (vector-ref v 0) (vector-ref v 1))))
       ;; the score renderer draws `image`s, never `arm_diagram` -- so convert the
       ;; poses here, and dances "just work" over the staff like the notes do.
       ;; (Needs `lib("programmart/dance.rhm")` imported for arm_diagram_to_image.)
       (if (zero? (hash-count dancers)) "" "arm_diagram_to_image\n")))

    ;; read AS code when the file is run: the forms wrapped in one `at []:` block
    ;; (a read-special result is a single term).  No lexical context
    ;; (`datum->syntax #f`) so the enclosing module binds `at`/`note`/`arm_diagram`.
    (define/public (read-special src line col pos)
      (datum->syntax
       #f
       (syntax->datum
        (parse-all (open-input-string (art->block (send this ->art-string)))
                   #:source src))))))

(define (copy-dancers dancers)
  (define h (make-hash))
  (for ([(k v) (in-hash dancers)]) (hash-set! h k (vector-copy v)))
  h)

;; --- the snip class (persistence) ---------------------------------------
(define score-snip-class%
  (class snip-class%
    (super-new)
    (define/override (read f)
      (define notes (make-hash))
      (define n (send f get-exact))
      (for ([_ (in-range n)])
        (define c (send f get-exact))
        (define r (send f get-exact))
        (hash-set! notes (cons c r) #t))
      (define dancers (make-hash))
      ;; dancers were added in version 2; older files simply have none
      (with-handlers ([exn:fail? void])
        (define d (send f get-exact))
        (for ([_ (in-range d)])
          (define col (send f get-exact))
          (define l (send f get-exact))
          (define r (send f get-exact))
          (define fi (send f get-exact))
          (hash-set! dancers col (vector l r (index->facing fi)))))
      ;; size the grid to fit its content (cols is not stored -- see write)
      (define used (apply max -1 (append (map car (hash-keys notes)) (hash-keys dancers))))
      (new score-snip% [notes notes] [dancers dancers]
           [cols (max DEFAULT-COLS (+ used 4))]))))

(define score-snip-class (new score-snip-class%))
(send score-snip-class set-classname "score-dance-editor:score")
(send score-snip-class set-version 2)
(send (get-the-snip-class-list) add score-snip-class)

;; the name DrRacket looks up to auto-load this class when reading a file
(define snip-class score-snip-class)
