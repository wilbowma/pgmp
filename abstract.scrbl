#lang scribble/base
@;#lang scribble/sigplan
@(require "defs.rkt")
@section{Abstract}
Profile directed optimization is a compiler technique that uses sample data
gathered at run-time to recompile and further optimize a program. The profile
data can be more accurate than heuristics normally used in a compiler and thus
can lead to more optimized code. Modern compilers like .NET, LLVM, and
GCC use profile directed optimization by profiling the low level code
and performing low level optimizations, such as reordering basic blocks.

Modern languages such as Haskell, C++, and Scheme provide powerful
meta-programming facilities that help programmers create generic
libraries, new language constructs, or even domain specific
languages. Meta-programs can manipulate source programs in way
reminiscent of a compiler. This paper presents a system for using
profile information to optimize programs in the meta-programming
language. The system is implemented and used in a high-performance
implementation of Scheme. 
