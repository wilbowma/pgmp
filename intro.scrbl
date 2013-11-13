#lang scribble/base
@(require scribble/manual)
@(require "defs.rkt")
@(require "bib.rkt")
@title[#:tag "intro" "Introduction"]
@; Introduce meta-programming
Meta-programs, or programs that write other programs, are often
used to implement high-level abstractions ranging from simple
syntactic abstractions, to compiler generators, to domain-specific
languages (DSLs). To name a few, C, C++, Haskell, Scheme, ML, and Scala
have support for meta-programming @~cite[taha00 erdweg11
czarnecki04 sheard02 dybvig93 burmako2013scala].
Ideally, meta-programs would not be concerned with generating
optimized code but instead leave that to the target-language compiler.
However, information is sometimes lost or obscured during the
translation into the target-language program.
For example, constraints on types, ranges, and effects can be lost,
as can the lack of constraints on data representation, algorithms,
and evaluation order.
Optimizations that depend on the lost information cannot be performed
by the target-language compiler and thus must be performed by the
meta-program, if at all.

@; NB: What is profile-guided optimization?
Profile-guided optimization (PGO) is a compiler technique that uses data
gathered at run-time on representative inputs to recompile and generate
optimized code. The code generated by this recompilation usually
exhibits improved performance on that class of inputs than the code
generate with static optimization heuristics. For instance, a compiler
can decide which loops to focus on unrolling based on which loops are
executed more frequently. Many compilers such as .NET, GCC, and LLVM use
profile-guided optimizations. The profile information used by these compilers,
such as execution counts of basic blocks or control flow graph nodes, is
low-level compared to the source-language operated on by meta-programs.
So the optimizations that use the profile information are also performed
on low-level constructs. Common optimizations include reordering basic
blocks, inlining decisions, conditional branch optimization, and
function layout decisions. 

Many compiler optimizations can benefit from the availability of
profile information and many contemporary compilers provide
support for gathering and using profile information for this
purpose.
Profile information can have an even greater impact on meta-program
optimizations.
For example, a meta-program might select data structures and
algorithms based on the frequency with which certain operations
are performed, potentially even leading to improvements in asymptotic
performance.

Existing techniques that use profile information
for these kinds of meta-program optimizations introduce a custom
toolchain, or expect the programmer to optimize code by hand. Chen et.
al. implement their own profile and meta-program toolchain to provide a
profile-guided meta-program for performing process placement for SMP
clusters@~citea["chen06"]. Liu and Rus provide a toolset that uses
profile information find suboptimial usage of the C++ STL, but leaves it
up to the programmer to make these changes@~citea["liu09"]. Hawkins et.
al. implement a compiler that generates C++ implementations of data
structures based on high-level specifications@~citea["hawkins11"
"hawkins12"]. These works implement highly specific meta-programming
or profiling systems to provide very advanced optimizations.  Yet no
general-purpose mechanism has been proposed to date that makes profile
information available to meta-programs for these kinds of optimizations.

This paper describes such a mechanism.
The mechanism makes profile information available at the granularity
of arbitrary target-language source points identified by the
meta-program.
In the case of a meta-program implementing an embedded DSL, these
could correspond to source expressions already present in the
source-language program.
In a manner similar to standard profile-guided optimization mechanisms,
making use of this mechanism involves running the meta-program and
compiler once to instrument the code, running the resulting
executable one or more times on representative data to gather
profile data, and running the meta-program and compiler a second time
to generate the optimized code.
During the second run of the meta-program, the meta-program retrieves
the profile information associated with source points.
The profile information is also available to the target-language compiler to
support the optimizations it performs.
The mechanism uses standard and efficient block-level profiling
techniques and is potentially suitable for dynamic optimization of
a running program in systems that support dynamic
recompilation@~cite[burger98]. It enables using data sets from multiple
executions of the instrumented program, and works with traditional
(``low-level'') PGO.

This mechanism has been implemented as part of a high performance Scheme
system, with profile information made available via an API
accessible from the high-level syntactic abstraction facility through
which Scheme supports meta-programming.  It would be straightforward to
adapt to most meta-programming systems with compilers that already
support profiling.

The reminder of the paper is organized as follows. @Secref{design}
presents the design of our system at a high level. 
@Secref{examples} demonstrates how to use our mechanism to implement several
optimizations as meta-programs. These example demonstrate how our
work can be used to implement and build on past work in a single,
general system. In particular, we show our work could be used to
automate the recommendataions produced by Liu and Rus by
automatically specialize an abstract sequence datatype@~citea{liu09}. We also
demonstrate how to implement profile-guided receiver class
prediction using our mechanism@~citea{grove95}. @Secref{implementation}
discusses our implementation and how it works with traditional PGOs.
