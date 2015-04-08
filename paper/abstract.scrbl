#lang scribble/base
@(require "defs.rkt")
Modern compilers systems such as GCC, .NET, and LLVM incorporate
profile-guided optimizations (PGOs) on low-level intermediate code and
basic blocks, with impressive results over purely static heuristics.
Recent work shows that profile information is also useful for performing
source-to-source optimizations via meta-programming.
For example, using profiling information to inform decisions about data
structures and algorithms can potentially lead to asymptotic
improvements in performance.

We present a design for supporting profile-guided meta-programs in a
general-purpose meta-programming system.
Our design is parametric over the particular profiling and
meta-programming systems.
We implement this design in two different meta-programming systems---the
syntactic extensions systems of Chez Scheme and Racket---and provide
several profile-guided meta-programs as usability case studies.
