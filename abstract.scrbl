#lang scribble/base
@(require "defs.rkt")
Modern compilers such as GCC, .NET, and LLVM incorporate profile-guided
optimizations (PGOs) on low-level intermediate code and basic blocks,
with impressive results over purely static heuristics.
Recent work shows that profile information is also useful for performing
source-to-source optimizations via meta-programming.
For example, using profiling information to inform decisions about data
structures and algorithms can potentially lead to asymptotic
improvements in performance.
General-purpose meta-programming systems should provide access to profile
information so meta-programmers can write their own,
potentially domain-specific optimizations.
Existing profile-guided meta-program come with their own
special-purpose toolkits, specific to a single optimization, creating
barriers to use and development of new profile-guided meta-programs.

We propose an approach for supporting profile-guided meta-programs in a
general-purpose meta-programming system.
Our approach is parametric over the particular profiling technique and
meta-programming system.
We demonstrate our approach by implementing it in two different
meta-programming system: the syntactic extensions
systems of Chez Scheme and Racket.
