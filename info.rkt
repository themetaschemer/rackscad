#lang info
(define collection "rackscad")
(define compile-omit-paths (list "src/tests" "examples"))
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/rackscad.scrbl" ())))
(define pkg-desc "Racket to OpenSCAD compiler")
(define version "0.1")
(define pkg-authors '("Anurag Mendhekar"))
