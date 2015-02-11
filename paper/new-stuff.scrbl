#lang scribble/base 
@(require "defs.rkt")
Meta-programs, or programs that write other programs, are often
used to implement high-level abstractions ranging from simple
syntactic abstractions, to compiler generators, to domain-specific
languages (DSLs).
In the ideal, meta-programs would not be concerned with generating
optimized code but instead leave that to the target-language compiler.
Information is sometimes lost or obscured, however, during the
translation into the target-language program.
For example, constraints on types, ranges, and effects can be lost,
as can the lack of constraints on data representation, algorithms,
and evaluation order.
Optimizations that depend on the lost information cannot be performed
by the target-language compiler and thus must be performed by the
meta-program, if at all.

Many compiler optimizations can benefit from the availability of
profile information to indicate the most commonly executed paths
through a program, and many contemporary compilers provide
support for gathering and using profile information for this
purpose.
Profile information can have an even greater impact on meta-program
optimizations.
For example, a meta-program might select data structures and
algorithms based on the frequency with which certain operations
are performed, potentially even leading to improvements in asymptotic
performance.
Yet no general-purpose mechanism has been proposed to date that makes
profile information available to meta-programs.

This paper describes such a mechanism.
The mechanism makes profile information available at the granularity
of arbitrary target-language source points identified by the
meta-program.
In the case of a meta-program implementing an embedded DSL, these
often correspond to source points already present in the source-language
program.
In a manner similar to standard profile-guided optimization mechanisms,
making use of this mechanism involves running the meta-program and
compiler once to instrument the code, running the resulting
executable one or more times on representative data to gather
profile data, and running the meta-program and compiler a second time
to generate the optimized code.
During the second run of the meta-program, the meta-program retrieves
the profile information via a database that associates source points
with profile data.
The database is also available to the target-language compiler to
support the optimizations it performs.
The mechanism uses standard and efficient block-level profiling
techniques and is potentially suitable for dynamic optimization of
a running program in systems that support dynamic recompilation.

This mechanism has been implemented as part of Chez Scheme, with
profile information made available via an API accessible from the
high-level syntactic abstraction facility through which Scheme
supports meta-programming.
It would be straightforward to adapt to most meta-programming systems
with compilers that already support profiling.
