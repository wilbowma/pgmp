#lang scribble/base
@(require
  scribble/manual
  scriblib/footnote
  "defs.rkt"
  "bib.rkt")

@title[#:tag "intro" "Introduction"]
@; What is profile-guided optimization?
Profile-guided optimization (PGO) is an optimization technique in which
a compiler uses profile information gathered at run time to improve the
performance of the generated code.
The profile information acts as an oracle for run-time behavior.
For example, a profiler might count how many times a program calls each
function to inform decisions about function inlining.
Compilers use profile information to guide decisions about reordering
basic blocks, function inlining, reordering conditional branches, and
function layout in memory@~citea{gupta02}.
Contemporary compiler systems that support PGO include GCC, .NET, and
LLVM@~cite[lattner02].
Code generated using PGOs usually exhibits improved performance, at least on
the represented class of inputs, compared to code generated with static
optimization heuristics.
For example, @citeta{Arnold:2000} show that using profiling information
to guide inlining decisions in Java resulted in up to 59% improvement
over static heuristics.
@todo{Not sure if I should cite .net and gcc documentation or not.}

@; Introduce profile-guided meta-programming.
Profile information has also proven useful to implement profile-guided
meta-programs.
@; Introduce meta-programming
Meta-programs are programs that operate on programs.
Languages with general-purpose meta-programming systems include C, C++,
Haskell@~citea{sheard2002template}, Java@~citea{erdweg11},
ML@~citea{taha00}, OCaml@~citea{bermetaocaml}, Racket@~cite[plt-tr1],
Scala@~citea{burmako2013scala}, and Scheme@~citea{dybvig93}.
Meta-programming is used to implement high-level yet efficient abstractions.
Boost libraries@~cite[boost] make heavy use of C++ meta-programming.
@; And profile-guided meta-programming
@citeta["sujeeth2014delite"] and @citeta["rompf10"] implement high-performance domain
specific languages using staged meta-programming in Scala.
@citeta{chen06:mpipp} implement process placement for SMP clusters using
profile-guided meta-programming.
@citeta{liu09} provide tools that use profile information to identify
suboptimal usage of the STL in C++ @nonbreaking{source code}.

@; Claim the state of the art is insufficient.
Current meta-programming systems do provide profile information to
meta-programs.
Therefore, to implement a profile-guided meta-program, programmers
must introduce new special-purpose toolkits for profiling and
meta-programming.
@; And propose a solution
Instead, meta-programming systems should provide access to profile
information from existing profilers.
Then programmers could reuse existing and familiar meta-programming
and profiling tools.

This paper presents a design for supporting profile-guided meta-programming in
general-purpose meta-programming systems.
To demonstrate the generality of our design, we implement it for
both Racket and Scheme.
Both implementations reuse existing meta-programming and
profiling infrastructure.

The rest of the paper is organized as follows.
In @Secref{example}, we introduce a running example and
Scheme-style meta-programming.
In @Secref{design}, we describe our requirements on the underlying
profiling system and an API for supporting profile-guided
meta-programming.
In @Secref{implementation}, we present two implementations of the
specification in @Secref{design}: one in Chez Scheme and one in Racket.
In @Secref{impl-other}, we sketch implementations for other
general-purpose meta-programming systems.
In @Secref{case-studies}, we demonstrate that our design is general
enough to implement and extend existing PGOs and profile-guided
meta-programs.
In @Secref{related}, we relate to existing work on PGOs and
profile-guided meta-programming.

The source code for our case studies and Racket implementation is
available online@note{http://dx.doi.org/10.5281/zenodo.16784}.
