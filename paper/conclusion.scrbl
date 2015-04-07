#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "conclusion" "Conclusion"]
Meta-programming is used to implement high-level optimizations, generate
code from high-level specifications, and create domain-specific
languages.
Each of these can take advantage of PGO to optimize before information
is lost or constraints are imposed.
Until now, such optimizations have been implemented via tools designed
for a specific meta-program or a specific optimization.
We described how to build a general mechanism for implementing arbitrary
profile-guided meta-programs.
We also demonstrated the expressivity of this design by by using it to
implement several examples of profile-guided meta-programs.
