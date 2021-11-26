#lang info
(define collection "text-table")
(define deps '("base" "rackunit"))
(define build-deps '("sandbox-lib"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/text-table.scrbl" ())))
(define pkg-desc "A simple package to print tables in utf-8/ascii format")
(define version "0.0")
(define pkg-authors '(laurent.orseau@gmail.com))
