#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     define-with-spec))

@title{Simple Function Specifications}
@author{@(author+email "Andrew Kent" "andmkent@iu.edu")}

@(define the-eval (make-base-eval))
@(the-eval '(require define-with-spec))

@defmodule[define-with-spec]

This package provides a simple form (@racket[define/spec])
for defining function definitions with specifications. This
macro is much simpler and not as expressive as the
@racket[define/contract] provided by Racket's contract
library, but error messages for same-module violations of
specifications will provide details about where the
violation occurred that the checks by
@racket[define/contract] do not.


@defform[
 #:id define/spec
 #:literals (-> any cons list listof either both except)
         (define/spec (name arg ...)
           (-> spec ... spec)
           body ...)
         #:grammar
         ([spec any
           #t
           #f
           symbol
           predicate
           (cons spec spec)
           (list spec ...)
           (listof spec)
           (either spec ...)
           (both spec ...)
           (except spec)])]{

 Defines a function @racket[name] equivalent to
 @racket[(define (name arg ...) body ...)] but at each place
 where @racket[name] is used, each provided argument is
 checked to ensure it passes its associated @racket[spec],
 and the return result is checked to ensure it passes the
 final @racket[spec].

 @examples[
 #:eval the-eval
 (define/spec (plus x y)
   (-> integer? integer? integer?)
   (+ x y))
 (plus 40 2)
 (eval:error
  (plus 40 "2"))
 (map plus '(1 2 3) '(4 5 6))
 (eval:error
  (map plus '(1 2 3) '(4 5 "6")))

 (define/spec (sum-all xs ys)
   (-> (listof integer?)
       (listof integer?)
       integer?)
   (foldl (λ (x y total) (plus (plus x y) total))
          0
          xs
          ys))
 (sum-all '(1 2 3) '(4 5 6))
 (eval:error
  (sum-all '(1 2 3) '(4 "5" 6)))

 (define/spec (sum-cars xs ys)
   (-> (listof (cons integer? any))
       (listof (cons integer? any))
       integer?)
   (foldl (λ (p1 p2 total) (plus (plus (car p1) (car p2)) total))
          0
          xs
          ys))
 (sum-cars '((1 "1") (2 "2") (3 "3"))
           '((4 "4") (5 "5") (6 "6")))
 ]
                            
}

@defform[(-> spec ... spec)]{
Only valid within a @racket[define/spec].

 A function with n arguments would require an initial
 sequence of n @racket[spec] that describe the arguments
 followed by a final @racket[spec] that describes the
 result.}

@defform[#:kind "spec" #:id any any]{
A specification that allows any value.
}

@defform[#:kind "spec" #:id symbol symbol]{ A literal symbol can be
 given to mean exactly that symbol value is permitted. }

@defform[#:kind "spec" #:id predicate predicate]{
Any identifier which is bound to a procedure
that accepts 1 argument and returns a boolean
can be used as a @racket[spec].

This can functions defined by @racket[racket]
(e.g. @racket[symbol?]), or any user defined
procedure.
}

@defform[#:kind "spec" (both spec ...)]{
Defines a
 @racket[spec] that only allows
 values that pass all of the provided
 @racket[spec]s (they are checked in order, left-to-right).

 e.g. @racket[(both integer? positive?)] is a @racket[spec]
 that only allows positive integers.
}


@defform[#:kind "spec" (either spec ...)]{
Defines a
 @racket[spec] that only allows
 values that pass at least one of the provided
 @racket[spec]s (they are checked in order, left-to-right).

 e.g. @racket[(either (both integer? positive?) string?)] is
 a @racket[spec] that allows positive integers or strings. }


@defform[#:kind "spec" (listof spec)]{
Used to described a list where each element
passes the @racket[spec].

e.g. @racket[(listof symbol?)] allows a list of symbols.
}


@defform[#:kind "spec" (cons spec spec)]{
Used to describe a pair where the
 @racket[car]
 passes the first @racket[spec]
 and the @racket[cdr]
 passes the second @racket[spec].

 e.g. @racket[(cons integer? string?)] will allow
 values that are a pair where the @racket[car]
 is an integer and the @racket[cdr] is a string.
}

@defform[#:kind "spec" (list spec ...)]{
Used to describe a list of fixed length where
each position @racket[spec] describes what
kids of value are permitted.

e.g. @racket[(list integer? string? (either string? #f))]
allows lists of length three where the first
element is an integer, the second a string,
and the third is either a string or @racket[#f].
}

@defform[#:kind "spec" (except spec)]{
Used to negate a
 @racket[spec].

 e.g. @racket[(both integer? (except negative?))]
 allows non-negative integers.
}