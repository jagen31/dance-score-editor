#lang racket/base

;; An embedded DrRacket editor for a score with dances.  On the staff, click a
;; cell to place a note; click-and-DRAG rightward to make it longer.  Click an
;; existing note's start to remove it.  Empty time becomes rests, so the score
;; is rhythm-accurate.  Above the staff is a dance lane (see below).  The grid
;; extends rightward without limit via the `+` strip at the right edge.
;;
;; Columns are 16th notes (a note spanning N columns is an N/16 note); the five
;; staff lines are the treble staff (E4 G4 B4 D5 F5).  Emits tonart art forms
;; via `->art-string`: `note` / `music_rest` come from `lib("tonart4/main.rhm")`,
;; the dancers need `lib("programmart/dance.rhm")` imported.
;;
;; Dancers: one per column in the lane; left-click a column to add one, click
;; near a placed dancer to aim an arm (left half = green/left, right half =
;; blue/right), click the strip under it to cycle facing, right-click to remove.

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
(define QUARTER-PER-COL 0.25)         ; a column is a 16th note (0.25 of a quarter)
(define FORMAT-TAG -1)                 ; sentinel marking the length-aware format

;; row -> (values pitch-letter octave).  Row 12 is E4 (bottom staff line).
(define LETTERS (vector "c" "d" "e" "f" "g" "a" "b"))
(define (row->pitch r)
  (define d (- 14 r))
  (values (vector-ref LETTERS (modulo d 7))
          (+ 4 (inexact->exact (floor (/ d 7))))))

;; a number of columns * 0.25 -> a clean interval endpoint string ("1", "0.25")
(define (q-str cols)
  (define x (* cols QUARTER-PER-COL))
  (if (integer? x) (number->string (inexact->exact x)) (number->string x)))

;; centre x of column c, and the dancer box for that column (snip-local)
(define (col-center c) (+ MARGIN (* c CELL-W) (/ CELL-W 2)))
(define (dbox-x c) (- (col-center c) (/ DANCER-W 2)))

(define LABEL-FONT (make-object font% 9 'default 'normal 'normal))
(define PLUS-FONT (make-object font% 18 'default 'normal 'bold))

;; --- the snip ------------------------------------------------------------
(define score-snip%
  (class* snip% (readable-snip<%>)
    (init-field [notes (make-hash)]       ; (cons col row) -> length in columns (>=1)
                [dancers (make-hash)]     ; col -> (vector l r facing-symbol)
                [cols DEFAULT-COLS])      ; how many 16th-note columns are shown
    (super-new)
    (inherit get-admin set-snipclass set-flags get-flags)
    (set-snipclass score-snip-class)
    (set-flags (cons 'handles-events (get-flags)))

    (define drag-key #f)                 ; (cons col row) of the note being dragged

    (define (grid-right) (+ MARGIN (* cols CELL-W)))    ; x past the last column
    (define (width) (+ (grid-right) ADD-W))
    (define (note-end k) (+ (car k) (hash-ref notes k)))    ; end column of a note
    (define (max-note-end) (for/fold ([m 0]) ([k (in-hash-keys notes)]) (max m (note-end k))))
    ;; the highest column any content reaches (for auto-sizing the grid)
    (define (max-used-col)
      (apply max -1 (append (map (lambda (k) (sub1 (note-end k))) (hash-keys notes))
                            (hash-keys dancers))))
    (define (min-cols) (max GROW (+ (max-used-col) 2)))

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
      ;; faint column gridlines, full height, so notes/dancers snap to 16ths
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
      ;; notes: a piano-roll bar spanning the note's duration
      (send dc set-brush "black" 'solid)
      (send dc set-pen "black" 1 'solid)
      (for ([(k len) (in-hash notes)])
        (define nx (+ x MARGIN (* (car k) CELL-W) 1))
        (define ny (+ y GRID-TOP (* (cdr k) CELL-H) (- (quotient CELL-H 2))))
        (send dc draw-rounded-rectangle nx ny (- (* len CELL-W) 2) CELL-H 3))
      ;; the `+` strip at the right edge (right-click trims)
      (define ax (+ x (grid-right)))
      (send dc set-brush (make-object color% 240 240 240) 'solid)
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-rectangle (+ ax 3) (+ y GRID-TOP) (- ADD-W 6) (- H GRID-TOP MARGIN))
      (send dc set-font PLUS-FONT)
      (send dc set-text-foreground (make-object color% 120 120 120))
      (send dc draw-text "+" (+ ax 8) (+ y GRID-TOP 6)))

    ;; --- hit testing -------------------------------------------------------
    (define (in-add-zone? ex) (>= ex (grid-right)))
    (define (col-of ex) (inexact->exact (floor (/ (- ex MARGIN) CELL-W))))
    (define (row-of ey) (inexact->exact (round (/ (- ey GRID-TOP) CELL-H))))
    (define (valid-cell? c r) (and (>= c 0) (< c cols) (>= r 0) (< r ROWS)))
    ;; the note whose START is (c . r), or #f
    (define (note-start-at c r) (and (hash-ref notes (cons c r) #f) (cons c r)))
    (define (col-at ex)
      (define c (col-of ex))
      (and (>= c 0) (< c cols) c))
    (define (dancer-hit ex)
      (for/fold ([best #f] [bd +inf.0] #:result best)
                ([col (in-list (hash-keys dancers))])
        (define d (abs (- ex (col-center col))))
        (if (and (<= d (/ DANCER-W 2)) (< d bd)) (values col d) (values best bd))))

    (define (edit-arm! col ex ey)
      (define v (hash-ref dancers col))
      (define-values (which hour)
        (dancer-arm-target (dbox-x col) MARGIN DANCER-W DANCER-BH (vector-ref v 2) ex ey))
      (if (eq? which 'l) (vector-set! v 0 hour) (vector-set! v 1 hour)))

    (define/override (on-event dc x y editorx editory evt)
      (define ex (- (send evt get-x) x))
      (define ey (- (send evt get-y) y))
      (cond
        ;; --- dragging a note longer/shorter -------------------------------
        [(and drag-key (send evt dragging?))
         (define c (col-of ex))
         (define new-len (max 1 (add1 (- c (car drag-key)))))
         (unless (= new-len (hash-ref notes drag-key))
           (hash-set! notes drag-key new-len) (refresh))]
        [(and drag-key (send evt button-up? 'left)) (set! drag-key #f)]
        ;; --- right click --------------------------------------------------
        [(send evt button-down? 'right)
         (cond
           [(in-add-zone? ex) (set! cols (max (min-cols) (- cols GROW))) (resized!)]
           [(< ey GRID-TOP)
            (define hit (dancer-hit ex))
            (when hit (hash-remove! dancers hit) (refresh))])]
        ;; --- left click ---------------------------------------------------
        [(send evt button-down? 'left)
         (cond
           [(in-add-zone? ex) (set! cols (+ cols GROW)) (resized!)]
           ;; the dance lane
           [(< ey GRID-TOP)
            (define hit (dancer-hit ex))
            (cond
              [(and hit (>= ey (+ MARGIN DANCER-BH)))
               (define v (hash-ref dancers hit))
               (vector-set! v 2 (index->facing (add1 (facing->index (vector-ref v 2)))))
               (refresh)]
              [hit (edit-arm! hit ex ey) (refresh)]
              [else
               (define c (col-at ex))
               (when (and c (not (hash-ref dancers c #f)))
                 (hash-set! dancers c (vector 6 6 'towards)) (refresh))])]
           ;; the staff: place / drag / remove a note
           [else
            (define c (col-of ex))
            (define r (row-of ey))
            (when (valid-cell? c r)
              (cond
                [(note-start-at c r)          ; click a note's start: remove it
                 (hash-remove! notes (cons c r)) (refresh)]
                [else                          ; new note; drag rightward to lengthen
                 (hash-set! notes (cons c r) 1)
                 (set! drag-key (cons c r))
                 (refresh)]))])]))

    (define/override (copy)
      (new score-snip% [notes (hash-copy notes)] [dancers (copy-dancers dancers)] [cols cols]))

    ;; --- persistence: a `-1` sentinel + version marks this length-aware format,
    ;; so an older (v1/v2) file -- which starts with a note count >= 0 -- is still
    ;; read (as length-1 notes) instead of overreading.
    (define/override (write f)
      (send f put FORMAT-TAG)
      (send f put 3)
      (define ks (sort (hash-keys notes)
                       (lambda (a b) (or (< (car a) (car b))
                                         (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
      (send f put (length ks))
      (for ([k (in-list ks)])
        (send f put (car k)) (send f put (cdr k)) (send f put (hash-ref notes k)))
      (define ds (sort (hash-keys dancers) <))
      (send f put (length ds))
      (for ([col (in-list ds)])
        (define v (hash-ref dancers col))
        (send f put col) (send f put (vector-ref v 0)) (send f put (vector-ref v 1))
        (send f put (facing->index (vector-ref v 2)))))

    ;; the score as tonart art forms: notes with real durations, rests filling
    ;; the gaps, and a dancer per pose placed at its column's beat.
    (define/public (->art-string)
      (define note-str
        (apply string-append
               (for/list ([k (in-list (sort (hash-keys notes)
                                            (lambda (a b) (< (car a) (car b)))))])
                 (define len (hash-ref notes k))
                 (define-values (letter oct) (row->pitch (cdr k)))
                 (format "at [interval ~a ~a]: note ~a 0 ~a\n"
                         (q-str (car k)) (q-str (+ (car k) len)) letter oct))))
      ;; rests: maximal runs of columns no note covers, within [0, last note end)
      (define covered (make-hash))
      (for ([(k len) (in-hash notes)])
        (for ([c (in-range (car k) (+ (car k) len))]) (hash-set! covered c #t)))
      (define end (max-note-end))
      (define rest-str
        (let loop ([c 0] [run #f] [acc ""])
          (define (flush s) (if run (string-append acc (format "at [interval ~a ~a]: music_rest\n"
                                                               (q-str run) (q-str c))) acc))
          (cond
            [(>= c end) (flush acc)]
            [(hash-ref covered c #f) (loop (add1 c) #f (flush acc))]
            [else (loop (add1 c) (or run c) acc)])))
      (define dance-str
        (apply string-append
               (for/list ([col (in-list (sort (hash-keys dancers) <))])
                 (define v (hash-ref dancers col))
                 (format "at [facing ~a]: at [interval ~a ~a]: arm_diagram ~a ~a\n"
                         (vector-ref v 2) (q-str col) (q-str (add1 col))
                         (vector-ref v 0) (vector-ref v 1)))))
      (string-append note-str rest-str dance-str
                     ;; poses -> images so dances render over the staff like notes
                     (if (zero? (hash-count dancers)) "" "arm_diagram_to_image\n")))

    ;; read AS code when the file is run: the forms wrapped in one `at []:` block
    ;; (a read-special result is a single term).  No lexical context so the
    ;; enclosing module binds `at`/`note`/`music_rest`/`arm_diagram`.
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
      (define dancers (make-hash))
      (define first (send f get-exact))
      (cond
        ;; length-aware format: FORMAT-TAG, version, then notes-with-length
        [(negative? first)
         (send f get-exact)                 ; version (unused for now)
         (define n (send f get-exact))
         (for ([_ (in-range n)])
           (define c (send f get-exact))
           (define r (send f get-exact))
           (define len (send f get-exact))
           (hash-set! notes (cons c r) (max 1 len)))
         (define d (send f get-exact))
         (for ([_ (in-range d)])
           (define col (send f get-exact)) (define l (send f get-exact))
           (define r (send f get-exact)) (define fi (send f get-exact))
           (hash-set! dancers col (vector l r (index->facing fi))))]
        ;; older format: `first` is the note count; notes are length-1 cells
        [else
         (for ([_ (in-range first)])
           (define c (send f get-exact))
           (define r (send f get-exact))
           (hash-set! notes (cons c r) 1))
         (with-handlers ([exn:fail? void])   ; dancers (v2) may be absent (v1)
           (define d (send f get-exact))
           (for ([_ (in-range d)])
             (define col (send f get-exact)) (define l (send f get-exact))
             (define r (send f get-exact)) (define fi (send f get-exact))
             (hash-set! dancers col (vector l r (index->facing fi)))))])
      (define used (apply max -1 (append (map (lambda (k) (+ (car k) (hash-ref notes k) -1))
                                              (hash-keys notes))
                                          (hash-keys dancers))))
      (new score-snip% [notes notes] [dancers dancers]
           [cols (max DEFAULT-COLS (+ used 2))]))))

(define score-snip-class (new score-snip-class%))
(send score-snip-class set-classname "score-dance-editor:score")
(send score-snip-class set-version 3)
(send (get-the-snip-class-list) add score-snip-class)

;; the name DrRacket looks up to auto-load this class when reading a file
(define snip-class score-snip-class)
