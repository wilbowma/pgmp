#lang scribble/base
@(require "defs.rkt")
Using profiling information to guide low-level optimizations has proven
beneficial.
Compilers such as GCC, .NET, and LLVM incorporate profile-guided
optimizations (PGOs) on low-level intermediate code and basic blocks.
Recent work has shown profile information is also useful for optimizing
source programs via meta-programming, i.e., writing programs that
perform source-to-source transformations.
For example, using profiling information to inform decisions about data
structures and algorithms can potentially lead to asymptotic
improvements in performance.
Unfortunately, no general-purpose meta-programming system makes profile
information available to the meta-programmer.
Each existing profile-guided meta-program comes with its own toolkit,
creating barriers to adopting and development.

We propose a approach for supporting multiple profile-guided
meta-program optimizations in a single general-purpose system.
Our approach uses fine-grained profile information, while making
use of standard and efficient basic block-level profile-instrumentation
techniques.
We have implemented our approach in Chez Scheme and Racket.
