#lang info
(define collection "redex-abbrevs")
(define deps '("redex-lib" "base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-doc" "redex-doc"))
(define pkg-desc "")
(define version "0.1")
(define pkg-authors '(ben))
(define scribblings '(("docs/redex-abbrevs.scrbl" () (omit-start))))
