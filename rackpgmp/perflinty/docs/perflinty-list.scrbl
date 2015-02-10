#lang scribble/manual
@(require
  (for-label
     (prefix-in builtin: racket/base)
     "../list.rkt"))

@title{Perflinty Lists}

@defmodule[perflinty/list]

This section describes a list library that warns the user when
operations that are asymptotically fast on vectors are being called on
instances of a list more often than operations that are asymptotically
fast on lists.

@defform[(list v ...)]{
Like @racketlink[builtin:list @racketfont{list}], but not a
@racket[procedure?] and constructs a separately profiled instance of a
Perflinty @racket[list].

Note that currently Racket @racketlink[builtin:list @racketfont{list}]s
and Perflinty @racket[list]s are incompatible.

@racket[list] does not affect @tech{score}s.
}


@defproc[(list? [ls? any/c]) boolean?]{
Like @racketlink[builtin:list? @racketfont{list?}], but only returns
@racket[#t] when @racket[ls] is a Perflinty @racket[list?].

@racket[list?] does not affect @tech{score}s.
}

@defproc[(map [proc? procedure?] [pls list?]) list?]{
Like @racketlink[builtin:map @racketfont{map}], but for Perflinty
@racket[list]s.

@racket[map] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(car [pls list?]) any/c]{
Like @racketlink[builtin:car @racketfont{car}], but for Perflinty
@racket[list]s.

@racket[car] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(cdr [pls list?]) list?]{
Like @racketlink[builtin:cdr @racketfont{cdr}], but for Perflinty
@racket[list]s.

@racket[cdr] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(cons [ele any/c] [pls list?]) list?]{
Like @racketlink[builtin:cons @racketfont{cons}], but for Perflinty
@racket[list]s.

@racket[cons] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(list-ref [pls list?] [pos exact-nonnegative-integer?])
any/c]{
Like @racketlink[builtin:list-ref @racketfont{list-ref}], but for Perflinty
@racket[list]s.

@racket[list-ref] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(length [pls list?]) exact-nonnegative-integer?]{
Like @racketlink[builtin:length @racketfont{length}], but for Perflinty
@racket[list]s.

@racket[length] adds 1 to the @racket[vector] @tech{score}.
}
