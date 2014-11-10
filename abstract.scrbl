#lang scribble/base
@(require "defs.rkt")
Modern compilers such as GCC, .NET, and LLVM incorporate profile-guided
optimizations (PGOs) on low-level intermediate code and basic blocks,
with impressive results over purely static heuristics.
Recent work shows that profile information is also useful for optimizing
source programs via meta-programming (writing programs that
perform source-to-source transformations).
For example, using profiling information to inform decisions about data
structures and algorithms can potentially lead to asymptotic
improvements in performance.
Meta-programming systems should makes profile information available to
to enable meta-programmers to write their own, potentially
domain-specific optimizations.
Currently, each existing profile-guided meta-program comes with its own
special-purpose toolkit, specific to a single optmization, creating
barriers to use and development of new profile-guided meta-programs.

We propose an approach for supporting profile-guided meta-programs in a
single general-purpose system.
Our approach is parametric over particular profiling technique and meta-programming
systems.
@todo{Ensure we make point about efficient, standard instrumentation
techniques in implementation section.}
We demonstrate our approach by implementing it in two different
(albeit similar) meta-programming system: Chez Scheme's macro system and
Racket's syntactic extension system.
