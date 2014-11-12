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

We have presented a general mechanism for profile-guided meta-program
optimizations implemented in Scheme. While our mechanism should easily
extend to other meta-programming facilities, we conclude by discussing
precisely how other common meta-programming facilities need to be
extended to use our mechanism.

Template Haskell, MetaOcaml, and Scala all feature powerful
meta-programming facilities similar to Scheme's@~cite[burmako2013scala
sheard02 dybvig93 taha00 czarnecki04]. They allow executing arbitrary
code at compile-time, provide quoting and unquoting of syntax, and
provide direct representations of the source AST. Source objects could
be attached to the AST, and @racket[profile-query-weight] could access
the source objects given an AST. These languages all appear to lack
source profilers, however.

C++ template meta-programming does not support running arbitrary
programs at compile time. This might limit the kinds of
optimizations that could be implemented using C++ template
meta-programming as it exists today. Many source level profilers already
exist for C++, so the challenge is in implementing source objects and
@racket[profile-query-weight]. C++ templates offers no way to directly
access and manipulate syntax, so it is not clear where to attach source
objects.
