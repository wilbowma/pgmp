#lang scribble/sigplan
@(require "defs.rkt")
Many languages, such as ML, C++, Haskell, Java, and Scheme, provide
powerful meta-programming facilities that help programmers create
generic libraries, new language constructs, and domain-specific
languages (DSLs). Meta-program manipulate source programs in ways
reminiscent of a compiler. In some ways, meta-programs serve the same
purpose as a compiler--to produce low-level code that is difficult to
reason about from the high-level code that is easy to understand. With
meta-programming increasingly prevalent, meta-programmers need some of
the same tools and techniques compiler writers have been developing for
decades. 

Profile directed optimization is a compiler technique that
uses sample data gathered at run-time to recompile and further optimize
a program. The profile data can be more accurate than static heuristics
normally used in a compiler and thus can lead to more optimized code.
Todays compilers like .NET, LLVM, and GCC all provide some kind of
profile directed optimizations.

This paper presents a system for using profile information to optimize
programs via meta-programming. The system is implemented and used in a
high-performance implementation of Scheme. 
