# define-with-spec
Racket macro for adding simple specifications to function definitions.

Specifications are enforced by code at each call site.

By design this macro is simpler and more limited than what is
offered by `racket/contract`, but it aims to provide more
detailed feedback about where local assumptions (i.e. within
a module) are violated.

## Usage:

```racket
(define/spec (fun-name arg ..._n)
   (-> spec ..._n spec)
   function-body ...)
```

where a `spec` is defined by the following grammar:

```
spec ::= identifier
         | any
         | (either spec ...)
         | (both spec ...)
         | (except spec)
         | (pair spec spec)
         | (list spec ...)
         | (listof spec)
         | boolean
         | symbol
```

Note: `identifier` in this case means the name of a function which
accepts a single argument and returns a boolean (e.g. number?).

i.e. the function name and argument list is followed by an
expression specifying the predicates that should
be used to check each input and the function's output.

## Limitations

1. Optional and keyword arguments are not supported.
2. `case-lambda` is not supported.
3. Combinators are limited to what is described by the aforementioned grammar.

## Disabling checks

To remove all runtime checks (i.e. make `define/spec` ignore its function specification and act exactly like `define`), add the following:

```racket
(require (for-syntax "path-to-define-with-spec.rkt"))
(begin-for-syntax (define/spec-enforcement? #f))
```
