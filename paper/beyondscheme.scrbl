#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual)

@title[#:tag "impl-other"]{Beyond Scheme and Racket}
Both of our implementations are for similar Scheme-style meta-programming
systems, but the approach should work in most meta-programming system.
Languages such as Haskell@~citea{sheard2002template}, MetaML@~citea{taha00},
and Scala@~cite[scala-overview-tech-report]
feature powerful meta-programming facilities similar to
that of Scheme@~cite[dybvig93].
They allow executing expressive programs at compile-time, provide direct
access to input expressions, and provide templating systems for
manipulating expressions.
In this section we briefly sketch implementation strategies for other
general-purpose @nonbreaking{meta-programming systems.}

@section{Template Haskell}
Template Haskell@~citea{sheard2002template} provides general-purpose
meta-programming to Haskell, and is provided with the current version of
the Glasgow Haskell Compilers (GHC).

GHC has a profiler that profiles "cost-centers".
By default, each function defines a cost-center, but users can define
new cost-centers by adding an @emph{annotation} to the source code of
the form @tt{{#- SCC "cost-centre-name" #-}}.
Cost-centers map easily to profile points.

Implementing our API using Template Haskell would be simple.
Template Haskell, as of GHC 7.7, supports generating and querying
annotations.
Since cost-centers are defined via annotations, implementing
@racket[make-profile-point], @racket[annotate-expr], and
@racket[profile-query] would be straightforward.
Implementing @racket[load-profile-info] is a simple matter of parsing
profile files.
The GHC profiler is called via a system call, and not inside the
language as in Chez Scheme and Racket.
Therefore, it would be useful to implement @racket[store-profile-info],
which stores profile information to a file.
Instead, profile information is stored to a file by the GHC profiler.

@section{MetaOCaml}
MetaOCaml provides general-purpose meta-programming based on multi-stage
programming for OCaml@~citea{bermetaocaml}.

OCaml features a counter-based profiler that associates counts with the
locations of certain source expressions.
To implement @racket[make-profile-point] and @racket[annotate-expr],
MetaOCaml would require the ability to manipulate source locations and
attach them to source expressions.
Then implementing @racket[profile-query] should be straightforward.
Like in Haskell, implementing @racket[load-profile-info] simply requires
parsing profile files, and profile information disk is stored to a file
outside of the language.

@section{Scala}
Scala features powerful general-purpose meta-programming, including
macros@~citea{burmako2013scala}, multi-stage
programming@~citea{rompf:2012:LMS}, and various reflection libraries.

Existing profilers for Scala work at the level of the JVM.
JVM bytecode retains much source information.
Therefore, it should be possible to map the profiling information at the
JVM level back to Scala source code.
With such a mapping, a Scala implementation of our API should be similar
to the implementation sketches for Haskell and MetaOCaml.
