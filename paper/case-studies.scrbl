#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual)

@title[#:tag "case-studies"]{Case Studies}
@todo{Is "expressive" the right word? I think so. I'm talking about what
kinds of of PGOs our design can express.}
To evaluate the expressive power and usability of our design, we carry
out three case studies.
In the first study, we demonstrate an implementation of @racket[case]
expressions, which are analogous to C's @code{switch} statements, that
performs a well-known PGO.
In the second study, we equip an embedded object system with profile-guided receiver
class prediction@~citea["holzle1994optimizing" "grove95"].
In the the third and final study, we present libraries that recommend
and automate high-level changes to data structures, similar to the
recommendations given by tools like Perflint@~citea{liu09}.

@include-section{study-case.scrbl}
@include-section{study-virtual-call.scrbl}
@include-section{study-datatype.scrbl}
