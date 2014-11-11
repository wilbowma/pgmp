#lang scribble/base
@(require
  scribble/manual
  "defs.rkt"
  "bib.rkt")

@title[#:tag "intro" "Introduction"]
@; What is profile-guided optimization?
Profile-guided optimization (PGO) is a compiler technique in which the
compiler uses profile information, e.g., execution counts, gathered
from test runs on representative sets of inputs to generate more
efficient code.
The profile information acts as an oracle for runtime behavior, and is
used to inform low-level optimization decisions about, e.g., reordering
basic blocks, inlining, reordering conditional branches, and function
layout@~citea{gupta02}.
The resulting code usually exhibits improved performance, at least on
the represented class of inputs, than code generated with static
optimization heuristics.
For example, using profiling information to inform inlining decisions in
Java resulted in up to 59% improvement over static
heuristics@~citea{Arnold:2000}.
Modern compilers that support PGO include .NET@~cite[.net], GCC@~cite[gcc], and
LLVM@~cite[lattner02].

@; Profile-directed meta-programming!
Profile information has also proven useful to implement profile-guided
meta-programs.
@; Introduce meta-programming
Meta-programs, i.e., programs that operate on programs, are
used to implement high-level abstractions such as
abstract libraries like Boost@~cite[boost],
compiler generators@~citea["keep2013nanopass"],
@todo{Should add some references to Delite, Scala work. Maybe in the
related work}
domain specific languages@~citea["sujeeth13" "flatt09"], and even whole
general purpose languages@~citea["rafkind12" "tobin-hochstadt11"
"tobin-hochstadt08" "barzilay05"].
Languages with existing meta-programming systems include
C, C++, Haskell@~citea{sheard2002template}, Java@~cite[erdweg11], ML@~cite[taha00 sheard02],
Racket@~cite[plt-tr1], Scheme@~cite[dybvig93], and
Scala.@~cite[burmako2013scala].

@;Not sure this should be in the same paragraph but ...
Using profile-guided meta-programming,
Chen et.  al. implement a profile-guided meta-program for performing
process placement for SMP clusters@~citea{chen06}.
Liu and Rus provide a tools that uses profile information to identify
suboptimal usage of the C++ STL @~citea{liu09}.
Hawkins et.  al. implement a compiler for a language that generates C++
implementations of data structures based on high-level
specifications@~citea["hawkins11" "hawkins12"].

Existing general-purpose meta-programming systems do not provide profile
information about the input programs on which the meta-program is
operating.
Therefore, existing profile-guided meta-programs introduce new
special-purpose toolkits for profiling and meta-programming.
@;Each of these new toolkits introduces a barrier to adoption, and
@;produces additional work for developers of new optimizations.
Instead, meta-programmers need an approach that gives existing
general-purpose meta-programming systems access to profile information.
Meta-programmers could then implement the profile-guided meta-programs,
reusing the meta-programming and profiling tools of an existing,
familiar, system.
Programmers could then take advantage of all the optimizations
implemented in that system.

We propose an approach for supporting profile-guided
meta-programming in a single general-purpose system.
The approach provides a simple API through which meta-programs can
access fine-grain source-level profile information, and does not
interfere with traditional, i.e., ``low-level'' PGOs.
@todo{Say something about profiling information being available for
run-time decisions, in later sections}

We implement this approach in Chez Scheme using standard and efficient
block-level profiling
techniques@~citea["ball1994optimally"]@~cite[burger98].
@;and is potentially suitable for dynamic
@;optimization of a running program in systems that support dynamic
@;recompilation@~cite[burger98].
We also implement this approach in Racket@~cite[plt-tr1] purely as a
library, using preexisting profiling tools.

The remainder of the paper is organized as follows.
In @secref{example} we present a running example and introduce
meta-programming in Scheme.
In @secref{design} we present the design of our system at a high-level,
and the implementation for both Chez Scheme and Racket. In
@secref{case-studies} we demonstrate that our approach is general enough
to implement and extend existing profile-guided optimizations and
profile-guided meta-programs.
In @secref{related} we related to existing work
on PGOs and profile-guided meta-programming.
We conclude in @secref{conclusion} a discussion of how our approach could
be implemented in other meta-programming systems.

The main contributions of the paper are: @itemlist[
  @item{A general approach for profile-guided meta-programming.}
  @item{An evaluation of this approach based on implementing existing
  profile-guided meta-programs, using our approach.}
]
@todo{Don't like the phrasing of that second bullet}
