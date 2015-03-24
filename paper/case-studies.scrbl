#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual)

@title[#:tag "case-studies"]{Case Studies}
In this section we evaluate the generality of our approach by
implementing existing PGOs and profile-guided meta-programs in the
Racket implementation of our approach.
First we demonstrate optimizing Scheme's @racket[case], a
multi-way branching construct similar to C's @code{switch}, as a
meta-program.
Then we implement profile-guided receiver class
prediction@~citea["holzle1994optimizing" "grove95"] for an
object system implemented as a syntax extension.
Finally we implement a sequence datatype that specializes each
instance to a @racket[list] or @racket[vector], based on
profiling information, automating the recommendations performed by tools
like Perflint@~citea{liu09}.

@include-section{study-case.scrbl}
@include-section{study-virtual-call.scrbl}
@include-section{study-datatype.scrbl}
