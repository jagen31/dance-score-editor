#lang info

(define collection "score-dance-editor")

;; rhombus-lib + the Art 4 stack are needed by art-anchor.rhm, which supplies
;; the lexical context that makes an embedded snip read as facade/danceart code.
(define deps '("base" "gui-lib" "draw-lib" "drracket-plugin-lib"
               "rhombus-lib" "facade-lib" "danceart-lib" "tonart4-lib"))
(define build-deps '())

(define version "0.0.1")
(define pkg-desc
  "Embedded DrRacket editors for a musical score and a dance (arm-diagram) pose.")
(define license '(MIT))

;; the DrRacket tool: adds an "Insert" menu with the two editor snips.
(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("Score & Dance Editor"))
(define drracket-tool-icons '(#f))
