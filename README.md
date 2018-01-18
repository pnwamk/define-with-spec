# define-with-spec

[![Build Status](https://travis-ci.org/pnwamk/define-with-spec.svg?branch=master)](https://travis-ci.org/pnwamk/define-with-spec)


Racket macro for adding simple specifications to definitions.

Specifications are enforced by code at each call site.

By design these macros are simpler and more limited than what is
offered by `racket/contract`, but they aim to provide more
detailed feedback about where local assumptions (i.e. within
a module) are violated.

See the scribble documenation for more details.
