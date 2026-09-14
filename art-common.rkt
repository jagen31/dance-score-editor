#lang racket/base

;; Shared helper for the embedded snips' `read-special`.
;;
;; A snip's `read-special` may only contribute ONE term where the snip sits, but
;; a snip can hold several art forms (a score is many notes).  Wrap them in a
;; single `at []:` block -- empty coordinates merge as the identity, so the block
;; just groups the forms into one term whose `at` the facade interpreter unfolds
;; into its (one or many) inner forms.  An empty snip becomes `at []:<< >>` (the
;; guillemet block form, the one shape of empty block shrubbery allows).

(provide art->block)

(define (art->block s)
  (define lines
    (filter (lambda (l) (not (regexp-match? #rx"^[ \t]*$" l)))
            (regexp-split #rx"\n" s)))
  (if (null? lines)
      (string-append "at []:" (string (integer->char 171)) " "
                     (string (integer->char 187)) "\n")   ; << >>  guillemets
      (apply string-append
             "at []:\n"
             (map (lambda (l) (string-append "  " l "\n")) lines))))
