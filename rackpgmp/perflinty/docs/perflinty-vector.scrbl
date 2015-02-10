#lang scribble/manual
@(require
   (for-label
     (prefix-in builtin: racket/base)
     (prefix-in builtin: racket/vector)
     (only-in racket/base list list?)
     "../vector.rkt"))

@title{Perflinty Vectors}
@defmodule[perflinty/vector]

This section describes a vector library that is like the
@racketmodname[perflinty/list] library, but for vectors. This library
warns the user when operations that are asymptotically fast on lists are
being called on instances of a vector more often than operations that
are asymptotically fast on vectors.

@defform[(vector v ...)]{
Like @racketlink[builtin:vector @racketfont{vector}], but not a
@racket[procedure], and constructs a separately profiled instance of a
Perflinty @racket[vector].

Note that currently Racket @racketlink[builtin:vector @racketfont{vector}]s
and Perflinty @racket[vector]s are incompatible.

@racket[vector] does not affect @tech{score}s.
}


@defproc[(vector? [v? any/c]) boolean?]{
Like @racketlink[builtin:vector? @racketfont{vector?}], but only returns
@racket[#t] when @racket[v?] is a Perflinty @racket[vector?].

@racket[vector?] does not affect @tech{score}s.
}

@defproc[(vector-map [proc? procedure?] [v vector?]) vector?]{
Like @racketlink[builtin:vector-map @racketfont{vector-map}], but for Perflinty
@racket[vectors]s.

@racket[vector-map] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(vector-ref [v vector?] [pos exact-nonnegative-integer?]) any/c]{
Like @racketlink[builtin:vector-ref @racketfont{vector-ref}], but for Perflinty
@racket[vector]s.

@racket[vector-ref] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(vector-set! [vec vector?] [pos exact-nonnegative-integer?] [v any/c])
void?]{
Like @racketlink[builtin:vector-set! @racketfont{vector-set!}], but for Perflinty
@racket[vector]s.

@racket[vector-set!] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(vector-length [v vector?]) exact-nonnegative-integer?]{
Like @racketlink[builtin:vector-length @racketfont{vector-length}], but for Perflinty
@racket[vector]s.

@racket[vector-length] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(vector-copy [v vector?]) vector?]{
Like @racketlink[builtin:vector-copy @racketfont{vector-copy}], but for Perflinty
@racket[vector]s.

@racket[vector-copy] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(vector-append [vec1 vector?] [vec2 vector?]) vector?]{
Like @racketlink[builtin:vector-append @racketfont{vector-append}], but for Perflinty
@racket[vector]s.

@racket[vector-append] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(vector->list [vec vector?]) list?]{
Like @racketlink[builtin:vector->list @racketfont{vector->list}], but for Perflinty
@racket[vector]s.

@racket[vector->list] adds 1 to the @racket[list] @tech{score}.
}
