#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     define-with-spec))

@title{Simple Function Specifications}
@author{@(author+email "Andrew Kent" "andmkent@iu.edu")}

@(define the-eval (make-base-eval))
@(the-eval '(require define-with-spec racket/match))

@defmodule[define-with-spec]

This package provides a few simple forms for creating
definitions with specifications. These macros are much
simpler and not as expressive as those provided by Racket's
contract library, but error messages for same-module
violations of specifications will provide details about
where the violation occurred that the standard
@racket[racket/contract] checks will not.

@section{Definitions with Specifications}

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

 (define/spec (bad-plus x y)
   (-> integer? integer? integer?)
   (number->string (+ x y)))
 (eval:error
  (bad-plus 40 2))
 ]
                            
}

@defform[
 #:id struct/spec
 (struct/spec name ([fld spec] ...))]{

 A @racket[struct/spec] form defines a structure data type
 @racket[name] with a constructor that enforces the given
 specification for each field. In other words, it defines the
 following procedures:

 @itemlist[@item{@racket[name], a constructor which takes as
             many arguments as there are @racket[fld]s in order to create
             a @racket[name] struct; each argument is checked against the
             associated @racket[fld]'s @racket[spec]. This identifier can
             also be used as a match-expander.}
           @item{@racket[name?], a predicate (i.e. a procedure
             of one argument) that returns @racket[#t] when given an
             instance of the structure type, otherwise it returns
             @racket[#f].}
           @item{@racket[name-fld], for each @racket[fld]; an accessor that
             takes an instance of the structure type and extracts the value
             for the corresponding field.}]

 @examples[
 #:eval the-eval
 (struct/spec posn ([x number?] [y number?]))

 (define origin (posn 0 0))
 (define p (posn 3 4))

 (eval:error
  (posn "3" "4"))

 (define/spec (distance p1 p2)
   (-> posn? posn? number?)
   (match* (p1 p2)
     [((posn x1 y1) (posn x2 y2))
      (sqrt (+ (expt (- x2 x1) 2)
               (expt (- y2 y1) 2)))]))

 (distance origin p)]
}

@section{Specification Constructors}

The following forms are used to construct @racket[spec]s:

@defform[(-> spec ... spec)]{
Used to describe the argument and result specifications for a procedure
 that is being defined (i.e. only valid within a @racket[define/spec]).

 A function with n arguments would require an initial
 sequence of n @racket[spec] that describe the arguments
 followed by a final @racket[spec] that describes the
 result.}

@defform[#:kind "spec" #:id any any]{
A specification that allows any value.
}

@defform[#:kind "spec" #:id symbol symbol]{ A literal symbol
 (e.g. @racket['apple]) can be given to mean exactly that
 symbol value is permitted. }

@defform[#:kind "spec" #:id predicate predicate]{
Any identifier which is bound to a procedure
that accepts 1 argument and returns a boolean
can be used as a @racket[spec].

 This can be a function defined by @racket[racket] (e.g.
 @racket[symbol?], @racket[exact-nonnegative-integer?], etc),
 or any user defined procedure. }

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
each positional @racket[spec] describes what
kinds of values are permitted in that position.

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

@section{Disabling Checks}

@defparam[define-with-spec-enforcement status boolean?
          #:value #t]{
Specification checking can be disabled in a module by
setting the @racket[define-with-spec-enforcement] parameter
(provided @racket[for-syntax] by @racket[define-with-spec])
to @racket[#f] at phase level 1.
}
