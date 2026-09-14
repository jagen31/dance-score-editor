#lang racket/base

;; The DrRacket tool: adds an "Insert Art" menu with the two embedded editors,
;; plus a command that copies the selected editor's art code to the clipboard.
;;
;; Requiring the two snip modules here registers their snip classes when the
;; tool loads, so inserted editors both display and persist in saved files.

(require racket/class
         racket/gui/base
         racket/unit
         drracket/tool
         ;; requiring these registers their snip classes (a load-time side
         ;; effect); we only name the snip% classes here
         (only-in "score-snip.rkt" score-snip%)
         (only-in "dance-snip.rkt" dance-snip%))

(provide tool@)

(define (art-snip? s) (or (is-a? s score-snip%) (is-a? s dance-snip%)))

;; put the selected editor snip's art code on the clipboard, if one is selected
(define (copy-selected-art txt)
  (define s (and txt (send txt find-next-selected-snip #f)))
  (when (and s (art-snip? s))
    (send the-clipboard set-clipboard-string
          (send s ->art-string) (current-milliseconds))))

(define (frame-mixin %)
  (class %
    (super-new)
    (inherit get-menu-bar get-definitions-text)
    (define menu (new menu% [label "Insert Art"] [parent (get-menu-bar)]))
    (new menu-item% [label "Score editor"] [parent menu]
         [callback (lambda (i e)
                     (send (get-definitions-text) insert (new score-snip%)))])
    (new menu-item% [label "Dance pose"] [parent menu]
         [callback (lambda (i e)
                     (send (get-definitions-text) insert (new dance-snip%)))])
    (new separator-menu-item% [parent menu])
    (new menu-item% [label "Copy selected editor's art code"] [parent menu]
         [callback (lambda (i e)
                     (copy-selected-art (get-definitions-text)))])))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame frame-mixin)))
