#lang racket/base

;; Requiring this registers both snip classes (so a saved file containing the
;; editors opens correctly) and re-provides the snip classes for programmatic
;; use -- e.g. `(send a-score-snip ->art-string)`.

(require (only-in "score-snip.rkt" score-snip% score-snip-class)
         (only-in "dance-snip.rkt" dance-snip% dance-snip-class))

(provide score-snip% score-snip-class
         dance-snip% dance-snip-class)
