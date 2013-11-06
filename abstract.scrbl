#lang scribble/base 
@(require "defs.rkt")
Many contemporary compilers allow the use of profile information
to guide various low-level optimizations.
This is not the case for contemporary meta-programming systems,
although profile information can have an even greater impact on the
high-level optimizations performed by meta-programs.
For example, a meta-program sometimes has control over the data
structures and algorithms used by the generated code, and use of
profiling information to select appropriate data structures and
algorithms can potentially lead even to asymptotic improvements in
performance.

This paper describes a mechanism for supporting profile-guided
meta-program optimization.
It makes profile information available at the granularity of arbitrary
target-language source points identified by the meta-program, while
making use of standard and efficient block-level profile-instrumentation
techniques.
We have implemented the mechanism as part of Chez Scheme, with
profile information made available via the syntactic abstraction
facility through which Scheme supports meta-programming.
The mechamism can be adapted to most meta-programming systems with
compilers that support profiling.
