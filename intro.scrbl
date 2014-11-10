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
Profile information has also proven useful to implement optimizing
meta-programs.
@; Introduce meta-programming
Meta-programs, i.e., programs that operate on programs, are
used to implement high-level abstractions such as
abstract libraries such as Boost@~cite[boost],
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
Meta-programmers could then implement the profile-guided meta-programs
in a single system, reusing the meta-programming and profiling tools of
an existing, familiar, system.
Programmers could then take advantage of all the optimizations
implemented in that system.

We propose an approach for supporting multiple profile-guided
meta-programs in a single general-purpose system. The approach provides
a simple API through which meta-programs can access fine-grain
source-level profile information, and does not interfere with
traditional, i.e., ``low-level'' PGOs.
@todo{Say something about profiling information being available for
run-time decisions, in later sections}

We implement this approach in Chez Scheme using standard and efficient
block-level profiling
techniques@~citea["ball1994optimally"]@~cite[burger98].
@;and is potentially suitable for dynamic
@;optimization of a running program in systems that support dynamic
@;recompilation@~cite[burger98].
We also implement this approach in Racket@~cite[plt-tr1] purely as a
library, using preexisting meta-programming facilities and profiling
libraries.

The remainder of the paper is organized as follows.
In @secref{example} we present a running example and introduce
meta-programming in Scheme.
In @secref{design/implementation} we present the design of our system at a high
level, and the implementation details for both Chez Scheme and Racket.
In @secref{case-studies} we demonstrates that our approach is general
enough to easily implement and improve upon existing profile-guided
optimizations and profile-guided meta-programs.
In @secref{related} we give a more detailed discussion of existing work
on PGOs and profile-guided meta-programming, and how our approach
supports this work.
We conclude in @secref{conclusion} a discussion of how our approach could
be implemented in other meta-programming systems.

@;The remainder of the paper is organized as follows. @Secref{design}
@;presents the design of our system at a high level.
@;@Secref{examples} demonstrates how to use our mechanism to implement several
@;optimizations as meta-programs. These examples demonstrate how our
@;work can be used to implement and build on past work in a single,
@;general system. In particular, we show how our work could be used to
@;automate the recommendations produced by Liu and Rus by
@;automatically specialize an abstract sequence datatype@~citea{liu09}. We also
@;demonstrate how to implement profile-guided receiver class@~citea{grove95}
@;prediction using our mechanism. @Secref{implementation}
@;discusses our implementation and how it works with traditional PGOs.
@;@Secref{related} discusses PGO and meta-programming in more detail. We
@;conclude in @secref{conclusion} with a comparison to existing
@;profile-guided meta-programs and a discussion of how our work could
@;be implemented in other meta-programming systems.

The main contributions of our work are: @itemlist[
  @item{A proposed approach for general-purpose profile-guided
  meta-programming.}
  @item{An open-source implementation of this approach in Racket, using existing
  profiling and meta-programming tools.}
  @item{Open-source implementations of case studies demonstrating how past work can be
  implemented and improved using our approach.}
]
