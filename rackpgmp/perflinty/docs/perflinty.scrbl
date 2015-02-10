#lang scribble/manual

@title{Perflinty}
@(require
   (for-label
     (prefix-in builtin: racket/base)
     "../auto.rkt"
     "../vector.rkt"
     "../list.rkt"))

@declare-exporting[#:use-sources (racket/base)]
This guide describes a profile-guided data structure specialization
collection similar in spirit to
@hyperlink["https://dl.acm.org/citation.cfm?id=1545076"]{Perflint},
provided by the @racket[rackpgmp] package.

The collection currently supports a limited subset of
@racketlink[builtin:list @racketfont{list}] and
@racketlink[builtin:vector @racketfont{vector}] operations.

Each instance of a @racket[list], @racket[vector], or @racket[seq]
provided by the collection is separately profiled using a naïve scoring
system. Each instance keeps track of various @tech{score}s. A
@deftech{score} is kept for each potential data structure representation
of the instance. For example, a @racket[list] keeps two @tech{score}s:
one for @racket[list] operations and one for @racket[vector] operations.
Each @tech{score} measures how frequently operations that are
asymptotically fast for that data structure are used on the
instance.
The operations provided by the modules of this collection use and
increment these scores differently.

As currently implemented, these scores are not fine-grained and may not
provide an accurate heuristic.

@include-section{perflinty-list.scrbl}
@include-section{perflinty-vector.scrbl}
@include-section{perflinty-auto.scrbl}
