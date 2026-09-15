#lang racket/base

;; An embedded DrRacket editor for a score with dances.
;;
;; Staff: click a cell to place a note; click-and-DRAG rightward to lengthen it
;; (a piano-roll bar); click its start to remove it.  Right-click a note to add
;; a sharp (shift-right-click for a flat) -- `#`/`x`/`b`/`bb` shows to its left.
;; A `>` marks every C row, like a clef.  Empty time becomes rests, so the score
;; is rhythm-accurate.
;;
;; Controls: the top strip has `oct -`/`oct +` to shift the whole score by an
;; octave.  The `+` at the right edge adds 16th-note columns; the `+` at the
;; bottom adds lower rows (more range).  Right-click either `+` to trim.
;;
;; Dance lane (above the staff): left-click a column to add a dancer, click near
;; one to aim an arm (left half = green/left, right half = blue/right), click the
;; strip under it to cycle facing, right-click to remove.
;;
;; Columns are 16th notes; the five staff lines are the treble staff.  Emits
;; tonart art via `->art-string`: `note`/`music_rest`/`arm_diagram` come from
;; `lib("tonart4/main.rhm")` + danceart.  Poses are emitted RAW; to hang them
;; over an engraved score add `arm_diagram_to_image` (lib("programmart/dance.rhm"))
;; in your program -- the strudel realizer reads the raw poses directly.

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
(define DEFAULT-ROWS 17)
(define GROW-COLS 8)
(define GROW-ROWS 4)
(define CELL-W 22)
(define CELL-H 9)
(define MARGIN 12)
(define CTRL-H 22)                    ; top control strip (octave)
(define ADD-W 30)                     ; the `+`/`-` strip at the right edge
(define BOTTOM-H 20)                  ; the extend-down strip below the staff
(define DANCE-LANE-H 108)             ; room above the staff for dancers
(define DANCER-W 44)
(define DANCER-BH 90)
(define GRID-TOP (+ CTRL-H DANCE-LANE-H))    ; y where the note grid starts
(define STAFF-ROWS '(4 6 8 10 12))    ; rows drawn as staff lines
(define QUARTER-PER-COL 0.25)         ; a column is a 16th note
(define FORMAT-TAG -1)                 ; sentinel marking the length/rich format
(define FORMAT-VERSION 4)

;; row + octave shift -> (values pitch-letter octave).  Row 12 is E4.
(define LETTERS (vector "c" "d" "e" "f" "g" "a" "b"))
(define (row->pitch r oct-shift)
  (define d (- 14 r))
  (values (vector-ref LETTERS (modulo d 7))
          (+ 4 oct-shift (inexact->exact (floor (/ d 7))))))
(define (row-is-c? r) (= 0 (modulo (- 14 r) 7)))

(define (accidental->str a)
  (cond [(= a 1) "#"] [(= a 2) "x"] [(= a -1) "b"] [(= a -2) "bb"] [else ""]))

(define (q-str cols)
  (define x (* cols QUARTER-PER-COL))
  (if (integer? x) (number->string (inexact->exact x)) (number->string x)))

(define (col-center c) (+ MARGIN (* c CELL-W) (/ CELL-W 2)))
(define (dbox-x c) (- (col-center c) (/ DANCER-W 2)))

(define LABEL-FONT (make-object font% 9 'default 'normal 'normal))
(define PLUS-FONT (make-object font% 18 'default 'normal 'bold))
(define CTRL-FONT (make-object font% 11 'default 'normal 'normal))
(define ACC-FONT (make-object font% 9 'default 'normal 'bold))

;; --- the snip ------------------------------------------------------------
(define score-snip%
  (class* snip% (readable-snip<%>)
    ;; notes: (cons col row) -> (cons length-in-columns accidental)
    (init-field [notes (make-hash)]
                [dancers (make-hash)]     ; col -> (vector l r facing-symbol)
                [cols DEFAULT-COLS]
                [rows DEFAULT-ROWS]
                [oct-shift 0])            ; whole-score octave offset
    (super-new)
    (inherit get-admin set-snipclass set-flags get-flags)
    (set-snipclass score-snip-class)
    (set-flags (cons 'handles-events (get-flags)))

    (define drag-key #f)   ; a note being length-dragged
    (define arm-drag #f)   ; (cons col which) while a dancer's arm is dragged

    (define (nlen k) (car (hash-ref notes k)))
    (define (nacc k) (cdr (hash-ref notes k)))
    (define (set-nlen! k v) (hash-set! notes k (cons v (nacc k))))
    (define (set-nacc! k v) (hash-set! notes k (cons (nlen k) v)))

    (define (grid-right) (+ MARGIN (* cols CELL-W)))
    (define (staff-bottom) (+ GRID-TOP (* rows CELL-H)))
    (define (width) (+ (grid-right) ADD-W))
    (define (height) (+ (staff-bottom) BOTTOM-H))
    (define (note-end k) (+ (car k) (nlen k)))
    (define (max-note-end) (for/fold ([m 0]) ([k (in-hash-keys notes)]) (max m (note-end k))))
    (define (max-col)
      (apply max -1 (append (map (lambda (k) (sub1 (note-end k))) (hash-keys notes))
                            (hash-keys dancers))))
    (define (max-row) (apply max -1 (map cdr (hash-keys notes))))

    (define (refresh)
      (define a (get-admin))
      (when a (send a needs-update this 0 0 (width) (height))))
    (define (resized!)
      (define a (get-admin))
      (when a (send a resized this #t) (send a needs-update this 0 0 (width) (height))))

    (define/override (get-extent dc x y [w #f] [h #f] [descent #f]
                                 [space #f] [lspace #f] [rspace #f])
      (when w (set-box! w (exact->inexact (width))))
      (when h (set-box! h (exact->inexact (height))))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ;; control-strip button rectangles (snip-local), so draw + hit stay in sync
    (define OCT-MINUS (list MARGIN 3 18 (- CTRL-H 6)))
    (define OCT-PLUS  (list (+ MARGIN 96) 3 18 (- CTRL-H 6)))
    (define (in-rect? ex ey r)
      (and (>= ex (list-ref r 0)) (< ex (+ (list-ref r 0) (list-ref r 2)))
           (>= ey (list-ref r 1)) (< ey (+ (list-ref r 1) (list-ref r 3)))))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc set-brush "white" 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y (width) (height))
      ;; control strip: octave -/+
      (define (button r label)
        (send dc set-brush (make-object color% 240 240 240) 'solid)
        (send dc set-pen (make-object color% 200 200 200) 1 'solid)
        (send dc draw-rectangle (+ x (list-ref r 0)) (+ y (list-ref r 1))
              (list-ref r 2) (list-ref r 3))
        (send dc draw-text label (+ x (list-ref r 0) 5) (+ y (list-ref r 1) 1)))
      (send dc set-font CTRL-FONT)
      (send dc set-text-foreground "black")
      (button OCT-MINUS "-")
      (button OCT-PLUS "+")
      (send dc draw-text (format "oct ~a" (if (>= oct-shift 0) (format "+~a" oct-shift) oct-shift))
            (+ x MARGIN 24) (+ y 4))
      ;; column gridlines through the staff area
      (send dc set-pen (make-object color% 225 225 225) 1 'solid)
      (for ([c (in-range (add1 cols))])
        (define cx (+ x MARGIN (* c CELL-W)))
        (send dc draw-line cx (+ y GRID-TOP) cx (+ y (staff-bottom))))
      ;; dancers in the lane
      (send dc set-font LABEL-FONT)
      (send dc set-text-foreground (make-object color% 120 120 120))
      (for ([col (in-list (sort (hash-keys dancers) <))])
        (define v (hash-ref dancers col))
        (draw-dancer dc (+ x (dbox-x col)) (+ y CTRL-H) DANCER-W DANCER-BH
                     (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))
        (send dc draw-text (symbol->string (vector-ref v 2))
              (+ x (dbox-x col) 2) (+ y CTRL-H DANCER-BH)))
      ;; separator between lane and staff
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-line (+ x MARGIN) (+ y GRID-TOP -4) (+ x (grid-right)) (+ y GRID-TOP -4))
      ;; staff lines
      (send dc set-pen "black" 1 'solid)
      (for ([r (in-list STAFF-ROWS)])
        (define ry (+ y GRID-TOP (* r CELL-H)))
        (send dc draw-line (+ x MARGIN) ry (+ x (grid-right)) ry))
      ;; `>` C markers in the left margin
      (send dc set-font CTRL-FONT)
      (send dc set-text-foreground (make-object color% 80 80 80))
      (for ([r (in-range rows)] #:when (row-is-c? r))
        (send dc draw-text ">" (+ x 1) (+ y GRID-TOP (* r CELL-H) (- (quotient CELL-H 2)) -3)))
      ;; notes: piano-roll bars, with any accidental to the left
      (for ([(k v) (in-hash notes)])
        (define len (car v)) (define acc (cdr v))
        (define nx (+ x MARGIN (* (car k) CELL-W) 1))
        (define ny (+ y GRID-TOP (* (cdr k) CELL-H) (- (quotient CELL-H 2))))
        (send dc set-brush "black" 'solid) (send dc set-pen "black" 1 'solid)
        (send dc draw-rounded-rectangle nx ny (- (* len CELL-W) 2) CELL-H 3)
        (unless (= acc 0)
          (send dc set-font ACC-FONT) (send dc set-text-foreground "black")
          (send dc draw-text (accidental->str acc) (- nx 9) (- ny 2))))
      ;; right `+` strip (extend columns)
      (define ax (+ x (grid-right)))
      (send dc set-brush (make-object color% 240 240 240) 'solid)
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-rectangle (+ ax 3) (+ y GRID-TOP) (- ADD-W 6) (* rows CELL-H))
      (send dc set-font PLUS-FONT) (send dc set-text-foreground (make-object color% 120 120 120))
      (send dc draw-text "+" (+ ax 8) (+ y GRID-TOP 6))
      ;; bottom `+` strip (extend rows down)
      (define by (+ y (staff-bottom)))
      (send dc set-brush (make-object color% 240 240 240) 'solid)
      (send dc set-pen (make-object color% 200 200 200) 1 'solid)
      (send dc draw-rectangle (+ x MARGIN) (+ by 3) (- (* cols CELL-W) 0) (- BOTTOM-H 6))
      (send dc set-font CTRL-FONT) (send dc set-text-foreground (make-object color% 120 120 120))
      (send dc draw-text "+ lower range" (+ x MARGIN 6) (+ by 3)))

    ;; --- hit testing -------------------------------------------------------
    (define (in-add-zone? ex) (>= ex (grid-right)))
    (define (in-bottom? ey) (>= ey (staff-bottom)))
    (define (col-of ex) (inexact->exact (floor (/ (- ex MARGIN) CELL-W))))
    (define (row-of ey) (inexact->exact (round (/ (- ey GRID-TOP) CELL-H))))
    (define (valid-cell? c r) (and (>= c 0) (< c cols) (>= r 0) (< r rows)))
    (define (note-start-at c r) (and (hash-ref notes (cons c r) #f) (cons c r)))
    (define (col-at ex) (define c (col-of ex)) (and (>= c 0) (< c cols) c))
    (define (dancer-hit ex)
      (for/fold ([best #f] [bd +inf.0] #:result best)
                ([col (in-list (hash-keys dancers))])
        (define d (abs (- ex (col-center col))))
        (if (and (<= d (/ DANCER-W 2)) (< d bd)) (values col d) (values best bd))))
    ;; grab the arm on the clicked side; returns which arm ('l/'r) so the caller
    ;; can keep dragging it
    (define (edit-arm! col ex ey)
      (define v (hash-ref dancers col))
      (define-values (which hour)
        (dancer-arm-target (dbox-x col) CTRL-H DANCER-W DANCER-BH (vector-ref v 2) ex ey))
      (if (eq? which 'l) (vector-set! v 0 hour) (vector-set! v 1 hour))
      which)
    ;; update the grabbed arm to follow the cursor
    (define (drag-arm! col which ex ey)
      (define v (hash-ref dancers col))
      (define hour (dancer-arm-hour (dbox-x col) CTRL-H DANCER-W DANCER-BH (vector-ref v 2) which ex ey))
      (if (eq? which 'l) (vector-set! v 0 hour) (vector-set! v 1 hour)))

    (define/override (on-event dc x y editorx editory evt)
      (define ex (- (send evt get-x) x))
      (define ey (- (send evt get-y) y))
      (cond
        ;; dragging a dancer's arm
        [(and arm-drag (send evt dragging?))
         (drag-arm! (car arm-drag) (cdr arm-drag) ex ey) (refresh)]
        [(and arm-drag (send evt button-up? 'left)) (set! arm-drag #f)]
        ;; dragging a note's length
        [(and drag-key (send evt dragging?))
         (define new-len (max 1 (add1 (- (col-of ex) (car drag-key)))))
         (unless (= new-len (nlen drag-key)) (set-nlen! drag-key new-len) (refresh))]
        [(and drag-key (send evt button-up? 'left)) (set! drag-key #f)]
        ;; right click
        [(send evt button-down? 'right)
         (cond
           [(in-add-zone? ex) (set! cols (max GROW-COLS (+ (max-col) 2) (- cols GROW-COLS))) (resized!)]
           [(in-bottom? ey)
            (set! rows (max DEFAULT-ROWS (+ (max-row) 2) (- rows GROW-ROWS))) (resized!)]
           ;; right-click a note: +/- accidental (shift = flatten)
           [(and (>= ey GRID-TOP) (note-start-at (col-of ex) (row-of ey)))
            (define k (cons (col-of ex) (row-of ey)))
            (define d (if (send evt get-shift-down) -1 1))
            (set-nacc! k (max -2 (min 2 (+ (nacc k) d)))) (refresh)]
           ;; right-click a dancer: remove
           [(and (>= ey CTRL-H) (< ey GRID-TOP))
            (define hit (dancer-hit ex))
            (when hit (hash-remove! dancers hit) (refresh))])]
        ;; left click
        [(send evt button-down? 'left)
         (cond
           ;; control strip: octave
           [(< ey CTRL-H)
            (cond [(in-rect? ex ey OCT-MINUS) (set! oct-shift (sub1 oct-shift)) (refresh)]
                  [(in-rect? ex ey OCT-PLUS)  (set! oct-shift (add1 oct-shift)) (refresh)])]
           ;; right add-zone: extend columns
           [(in-add-zone? ex) (set! cols (+ cols GROW-COLS)) (resized!)]
           ;; bottom: extend rows down
           [(in-bottom? ey) (set! rows (+ rows GROW-ROWS)) (resized!)]
           ;; dance lane
           [(< ey GRID-TOP)
            (define hit (dancer-hit ex))
            (cond
              [(and hit (>= ey (+ CTRL-H DANCER-BH)))
               (define v (hash-ref dancers hit))
               (vector-set! v 2 (index->facing (add1 (facing->index (vector-ref v 2)))))
               (refresh)]
              [hit (set! arm-drag (cons hit (edit-arm! hit ex ey))) (refresh)]
              [else
               (define c (col-at ex))
               (when (and c (not (hash-ref dancers c #f)))
                 (hash-set! dancers c (vector 6 6 'towards)) (refresh))])]
           ;; staff: place / drag / remove a note
           [else
            (define c (col-of ex)) (define r (row-of ey))
            (when (valid-cell? c r)
              (cond
                [(note-start-at c r) (hash-remove! notes (cons c r)) (refresh)]
                [else
                 (hash-set! notes (cons c r) (cons 1 0))
                 (set! drag-key (cons c r)) (refresh)]))])]))

    (define/override (copy)
      (new score-snip% [notes (hash-copy notes)] [dancers (copy-dancers dancers)]
           [cols cols] [rows rows] [oct-shift oct-shift]))

    (define/override (write f)
      (send f put FORMAT-TAG) (send f put FORMAT-VERSION)
      (send f put oct-shift) (send f put rows)
      (define ks (sort (hash-keys notes)
                       (lambda (a b) (or (< (car a) (car b))
                                         (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
      (send f put (length ks))
      (for ([k (in-list ks)])
        (send f put (car k)) (send f put (cdr k)) (send f put (nlen k)) (send f put (nacc k)))
      (define ds (sort (hash-keys dancers) <))
      (send f put (length ds))
      (for ([col (in-list ds)])
        (define v (hash-ref dancers col))
        (send f put col) (send f put (vector-ref v 0)) (send f put (vector-ref v 1))
        (send f put (facing->index (vector-ref v 2)))))

    (define/public (->art-string)
      (define note-str
        (apply string-append
               (for/list ([k (in-list (sort (hash-keys notes) (lambda (a b) (< (car a) (car b)))))])
                 (define-values (letter oct) (row->pitch (cdr k) oct-shift))
                 (format "at [interval ~a ~a]: note ~a ~a ~a\n"
                         (q-str (car k)) (q-str (+ (car k) (nlen k))) letter (nacc k) oct))))
      (define covered (make-hash))
      (for ([k (in-hash-keys notes)])
        (for ([c (in-range (car k) (note-end k))]) (hash-set! covered c #t)))
      (define end (max-note-end))
      (define rest-str
        (let loop ([c 0] [run #f] [acc ""])
          (define (flush s) (if run (string-append s (format "at [interval ~a ~a]: music_rest\n"
                                                             (q-str run) (q-str c))) s))
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
      ;; NB: emit RAW `arm_diagram` poses -- do NOT auto-convert to images here.
      ;; Different realizers want different things: the strudel realizer reads the
      ;; raw poses, while program_png_pict wants images.  So the conversion is the
      ;; program's choice: add `arm_diagram_to_image` yourself when realizing to a
      ;; score (program_png_pict / program_scribbler); omit it for strudel.
      (string-append note-str rest-str dance-str))

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
(define (read-dancers f dancers)
  (define d (send f get-exact))
  (for ([_ (in-range d)])
    (define col (send f get-exact)) (define l (send f get-exact))
    (define r (send f get-exact)) (define fi (send f get-exact))
    (hash-set! dancers col (vector l r (index->facing fi)))))

(define score-snip-class%
  (class snip-class%
    (super-new)
    (define/override (read f)
      (define notes (make-hash))
      (define dancers (make-hash))
      (define oct-shift 0)
      (define rows DEFAULT-ROWS)
      (define first (send f get-exact))
      (cond
        [(negative? first)
         (define ver (send f get-exact))
         (when (>= ver 4)
           (set! oct-shift (send f get-exact))
           (set! rows (send f get-exact)))
         (define n (send f get-exact))
         (for ([_ (in-range n)])
           (define c (send f get-exact)) (define r (send f get-exact))
           (define len (send f get-exact))
           (define acc (if (>= ver 4) (send f get-exact) 0))
           (hash-set! notes (cons c r) (cons (max 1 len) acc)))
         (read-dancers f dancers)]
        [else
         (for ([_ (in-range first)])
           (define c (send f get-exact)) (define r (send f get-exact))
           (hash-set! notes (cons c r) (cons 1 0)))
         (with-handlers ([exn:fail? void]) (read-dancers f dancers))])
      (define used-c (apply max -1 (append (map (lambda (k) (+ (car k) (car (hash-ref notes k)) -1))
                                                (hash-keys notes))
                                            (hash-keys dancers))))
      (define used-r (apply max -1 (map cdr (hash-keys notes))))
      (new score-snip% [notes notes] [dancers dancers] [oct-shift oct-shift]
           [cols (max DEFAULT-COLS (+ used-c 2))] [rows (max rows DEFAULT-ROWS (+ used-r 2))]))))

(define score-snip-class (new score-snip-class%))
(send score-snip-class set-classname "score-dance-editor:score")
(send score-snip-class set-version 4)
(send (get-the-snip-class-list) add score-snip-class)

(define snip-class score-snip-class)
