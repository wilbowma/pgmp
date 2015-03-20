#lang scribble/base
@(require
  scribble/manual
  "defs.rkt"
  "bib.rkt")

@title[#:tag "intro" "Introduction"]
@; What is profile-guided optimization?
Profile-guided optimization (PGO) is a compiler technique in which a
compiler uses profile information, e.g., counts of how often each
expression is executed, gathered from test runs on representative sets
of inputs to generate more efficient code.
The profile information acts as an oracle for runtime behavior, and
informs low-level optimization decisions, e.g., decisions about
reordering basic blocks, inlining, reordering conditional branches, and
function layout in memory@~citea{gupta02}.
The resulting code usually exhibits improved performance, at least on
the represented class of inputs, compared to code generated with static
optimization heuristics.
For example, using profiling information to inform inlining decisions in
Java resulted in up to 59% improvement over static
heuristics@~citea{Arnold:2000}.
Modern compilers that support PGO include .NET, GCC, and
LLVM@~cite[lattner02].
@todo{Not sure if I should cite .net and gcc documentation or not.}

@; Profile-directed meta-programming!
Profile information has also proven useful to implement profile-guided
meta-programs.
@; Introduce meta-programming
Meta-programs, i.e., programs that operate on programs, are
used to implement high-level abstractions such as
efficient abstract libraries like Boost@~cite[boost] and
high-performance domain specific languages@~citea["sujeeth2014delite"
"rompf10"].
Using profile-guided meta-programming,
@citeta{chen06:mpipp} implement process placement for SMP clusters.
@citeta{liu09} provide tools that use profile information to identify
suboptimal usage of the STL in C++ source code.
Languages with general-purpose meta-programming systems include C, C++,
Haskell@~citea{sheard2002template}, Java@~citea{erdweg11},
ML@~citea{taha00}, Racket@~cite[plt-tr1],
Scheme@~cite[dybvig93], and Scala@~citea{burmako2013scala}.

Existing general-purpose meta-programming systems do not provide profile
information about the input programs on which meta-programs operate.
Therefore, existing profile-guided meta-programs introduce new
special-purpose toolkits for profiling and meta-programming.
Instead, general-purpose meta-programming systems should provide access
to profile information.
Meta-programmers could then implement profile-guided meta-programs while
reusing the meta-programming and profiling tools of an existing,
familiar, system.
Programmers could then take advantage of all the meta-programs
implemented in that system.

We propose an approach for supporting profile-guided
meta-programming in a general-purpose meta-programming system.
The approach provides a simple API through which meta-programs can
access fine-grain source-level profile information, and does not
interfere with traditional, i.e., ``low-level'' PGOs.
@todo{Say something about profiling information being available for
run-time decisions, in later sections}

We implement this approach in Chez Scheme using standard and efficient
block-level profiling
techniques@~citea["ball1994optimally" "burger1998infrastructure"].
We also implement this approach in Racket@~cite[plt-tr1] purely as a
library, using pre-existing profiling and meta-programming tools.

The remainder of the paper is organized as follows.
In @Secref{example}, we present a running example and introduce
Scheme-style meta-programming.
In @Secref{design}, we present the design of our approach and an example
of an API provided by a meta-programming system using our approach.
In @Secref{implementation}, we present two instantiations of our
approach, one in Chez Scheme and one in Racket.
In @Secref{case-studies}, we demonstrate that our approach is general
enough to implement and extend existing PGOs and profile-guided
meta-programs.
In @Secref{related}, we related to existing work on PGOs and
profile-guided meta-programming.

The main contributions of the paper are: @itemlist[
  @item{A general approach for profile-guided meta-programming.}
  @item{Two instantiations of our approach}
  @item{An evaluation of our approach based on implementing three
  existing profile-guided meta-programs.}
]
