#lang scribble/base
@(require scribble/manual)
@(require "defs.rkt")
@(require "bib.rkt")
@title[#:tag "intro" "Introduction"]
@; Introduce meta-programming
Meta-programs, or programs that operate on programs, are often
used to implement high-level abstractions ranging from simple
syntactic abstractions, to compiler generators, to domain-specific
languages (DSLs).
While meta-programs can be written for virtually any language,
some languages have built-in support for meta-programming,
including C, C++, Haskell, Scheme, ML, and Scala.@~cite[taha00
erdweg11 czarnecki04 sheard02 dybvig93 burmako2013scala].
Ideally, meta-programs would not be concerned with generating
optimized code but instead leave that to the target-language compiler.
However, information is sometimes unavoidably lost or obscured during the
translation into the target-language program.
For example, constraints on types, ranges, and effects can be lost,
as can the lack of constraints on data representation, algorithms,
and evaluation order.
Optimizations that depend on the lost information cannot be performed
by the target-language compiler and thus must be performed by the
meta-program, if at all.

@; NB: What is profile-guided optimization?
Profile-guided optimization (PGO) is a compiler technique in which
profile information, e.g., execution counts, from test runs on
representative sets of inputs is fed into the compiler to enable
it to generate more efficient code.
The resulting code usually
exhibits improved performance, at least on the represented class
of inputs, than code
generated with static optimization heuristics.
@; For instance, a compiler
@; can decide which loops to unroll based on which loops are
@; executed more frequently.
Compilers that support PGO include .NET, GCC, and LLVM@~cite[lattner02 gcc .net]
The profile information used by these compilers,
such as execution counts of basic blocks or control flow graph nodes, is
low-level compared to the source-language operated on by meta-programs,
so the optimizations that use the profile information are also performed
on low-level constructs. Common optimizations include reordering basic
blocks, inlining decisions, conditional branch optimization, and
function layout decisions@~citea{gupta02}.

Profile information can have an even greater impact on meta-program
optimizations.
For example, a meta-program might select data structures and
algorithms based on the frequency with which certain operations
are performed, potentially even leading to improvements in asymptotic
performance.

Existing techniques that use profile information
for these kinds of meta-program optimizations introduce a custom
toolchain, or expect the programmer to optimize code by hand. Chen et.
al. implement their own profile and meta-program tools to provide a
profile-guided meta-program for performing process placement for SMP
clusters@~citea{chen06}. Liu and Rus provide a tools that uses
profile information to identify suboptimial usage of the C++ STL, but leaves it
up to the programmer to take corrective action@~citea{liu09}. Hawkins et.
al. implement a compiler for a language that generates C++
implementations of data structures based on high-level
specifications@~citea["hawkins11" "hawkins12"]. These works implement
highly specific meta-programming or profiling systems to provide
advanced optimizations.  Yet no general-purpose mechanism has been
proposed to date that makes profile information available to
meta-programing systems for arbitrary optimizations.

This paper describes such a general-purpose mechanism.
Our mechanism makes profile information available at the granularity
of arbitrary source points identified by the
meta-program.
In the case of a meta-program implementing an embedded DSL, these
could correspond to source expressions already present in the
source-language program.
In a manner similar to standard profile-guided optimization mechanisms,
making use of our mechanism involves running the meta-program and
compiler once to instrument the code, running the resulting
executable one or more times on representative data to gather
profile data, and running the meta-program and compiler a second time
to generate the optimized code.
During the second run of the meta-program, the meta-program retrieves
the profile information associated with source points, and can use this
information to inform transformations and optimizations.
The profile information is also available to the target-language compiler to
support the further optimizations at the target-language level.

Our implementation of this mechanism in Chez Scheme uses standard and
efficient block-level profiling techniques and is potentially suitable
for dynamic optimization of a running program in systems that support
dynamic recompilation@~cite[burger98]. It enables using data sets from
multiple executions of the instrumented program, and does not interfere
with traditional (``low-level'') PGO.  We implement this mechanism as
part of a high performance Scheme system, with profile information made
available via an API accessible from the high-level syntactic
abstraction facility through which Scheme supports meta-programming, and
even accessible at run-time. It should be straightforward to adapt to
most meta-programming systems with compilers that already support
profiling.

We also reimplement this mechanism, and all our examples, in the Racket
programming langauge@~cite[plt-tr1]. While the meta-programming facilities
provided in Racket are similar to those of Chez Scheme, the language
implementation and profiling systems are entirely different. We're able to
reimplement our work by reusing the existing Racket profiling and
meta-programming infrastructure without making any changes to the language
implementation.

The remainder of the paper is organized as follows. @Secref{design}
presents the design of our system at a high level.
@Secref{examples} demonstrates how to use our mechanism to implement several
optimizations as meta-programs. These examples demonstrate how our
work can be used to implement and build on past work in a single,
general system. In particular, we show how our work could be used to
automate the recommendataions produced by Liu and Rus by
automatically specialize an abstract sequence datatype@~citea{liu09}. We also
demonstrate how to implement profile-guided receiver class@~citea{grove95}
prediction using our mechanism. @Secref{implementation}
discusses our implementation and how it works with traditional PGOs.
@Secref{related} discusses PGO and meta-programming in more detail. We
conclude in @secref{conclusion} with a discussion of how our work could
be implemented in other meta-programming systems.
