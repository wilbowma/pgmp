#lang scribble/manual
@(require
   (for-label
     (prefix-in builtin: racket/base)
     "../auto.rkt"
     "../vector.rkt"
     "../list.rkt"))

@title{Perflinty Auto}
@defmodule[perflinty/auto]

This section describes a sequence library that automatically
specializes each instance to a @racket[list] or
@racket[vector], depending on the @racket[list] and @racket[vector]
@tech{score}s of each instance of a @racket[seq].

@defform[(seq v ...)]{
Like a @racket[list] or @racket[vector], depending on the @racket[list]
and @racket[vector] @tech{score}s for this instance.
}

@defproc[(seq? [s? any/c]) boolean?]{
Returns @racket[#t] if @racket[s?] is a @racket[seq], and @racket[#f]
otherwise.

@racket[seq?] does not affect @tech{score}s.
}

@defproc[(seq-map [proc procedure?] [s seq?]) seq?]{
Like a @racket[map] or @racket[vector-map] but on @racket[seq]s.

@racket[seq-map] does not affect @tech{score}s.
}

@defproc[(seq-first [s seq?]) any/c]{
Like a @racket[car] or @racket[(vector-ref s 0)] but on @racket[seq]s.

@racket[seq-first] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(seq-rest [s seq?]) seq?]{
Like a @racket[cdr] but on @racket[seq]s.

@racket[seq-rest] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(seq-cons [ele any/c] [s seq?]) seq?]{
Like a @racket[cons] but on @racket[seq]s.

@racket[seq-cons] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(seq-append [s1 seq?] [s2 seq?]) seq?]{
Like a @racket[append] but on @racket[seq]s.

@racket[seq-append] adds 1 to the @racket[list] @tech{score}.
}

@defproc[(seq-copy [s seq?]) seq?]{
Like a @racket[vector-copy] but on @racket[seq]s.

@racket[seq-copy] does not affect @tech{score}s.
}

@defproc[(seq-ref [s seq?] [pos exact-nonnegative-integer?]) any/c]{
Like a @racket[vector-ref] but on @racket[seq]s.

@racket[seq-ref] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(seq-set! [s seq?] [pos exact-nonnegative-integer?] [v any/c]) void?]{
Like a @racket[vector-set!] but on @racket[seq]s.

@racket[seq-set!] adds 1 to the @racket[vector] @tech{score}.
}

@defproc[(seq-length [s seq?]) any/c]{
Like a @racket[vector-length] but on @racket[seq]s.

@racket[seq-length] adds 1 to the @racket[vector] @tech{score}.
}
