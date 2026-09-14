#lang info

(define collection "score-dance-editor")

;; shrubbery-lib supplies the parser used by each snip's read-special; the Art 4
;; forms bind in whatever module embeds the snip, so no art deps are needed here.
(define deps '("base" "gui-lib" "draw-lib" "drracket-plugin-lib" "shrubbery-lib"))
(define build-deps '())

(define version "0.0.1")
(define pkg-desc
  "Embedded DrRacket editors for a musical score and a dance (arm-diagram) pose.")
(define license '(MIT))

;; the DrRacket tool: adds an "Insert" menu with the two editor snips.
(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("Score & Dance Editor"))
(define drracket-tool-icons '(#f))
