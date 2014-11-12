#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "conclusion" "Conclusion"]
Meta-programming is being used to implement high-level optimizations,
generate code from high-level specifications, and create DSLs. Each of
these can take advantage of PGO to optimize before information is lost
or constraints are imposed. Until now, such optimizations have been
implemented via toolchains designed for a specific meta-program or
optimization. We have described a general mechanism for implementing
arbitrary profile-guided meta-program optimizations, and demonstrated
its use by implementing several optimizations previously implemented in
seperate, specialized toolchains.
