#lang info

(define collection "define-with-spec")
(define version "0.1")
(define deps '("base"))
(define pkg-desc "Simple Function Specifications.")

(define build-deps '("racket-doc" "scribble-lib" "rackunit-lib"))
(define scribblings '(("scribblings/define-with-spec.scrbl")))
