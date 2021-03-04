#lang info
(define collection "multi-id")
(define deps '("base"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "phc-toolkit"
               "type-expander"
               "scribble-lib"
               "hyper-literate"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "scribble-enhanced"
                     "typed-racket-doc"))
(define scribblings '(("scribblings/multi-id.scrbl" ())
                      ("multi-id.hl.rkt" () (omit-start))))
(define pkg-desc "Description Here")
(define version "0.9")
(define pkg-authors '(|Suzanne Soy|))
