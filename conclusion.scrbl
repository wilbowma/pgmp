#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "conclusion" "Conclusion"]
Meta-programming is being used to implement high-level optimizations,
generate code from high-level specifications, and create domain-specific
languages.
Each of these can take advantage of PGO to optimize before information
is lost or constraints are imposed.
Until now, such optimizations have been implemented via tools designed
for a specific meta-program or optimization.
We have described a general mechanism for implementing arbitrary
profile-guided meta-programs, and evaluated its use by implementing
several PGOs and profile-guided meta-programs in a single
general-purpose system.
