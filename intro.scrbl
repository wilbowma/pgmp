#lang scribble/base
@(require scribble/manual)
@(require "defs.rkt")
@section[#:tag "intro" "Introduction"]
Profile directed optimizations can enable better optimization by using
potentially more accurate run-time data than traditional heuristics.
Current techniques for doing profile directed optimization use low level
profile information, such as information about basic blocks. @todo{cite}At
this low level, much information about the program is lost, limiting
which optimizations the compiler can do. By bringing profile directed
optimizations up to the source level, programmers can use specific
knowledge of the problem domain, program, and the high-level code to
optimize programs in ways a compiler can not. 

A simple example is a conditional branching construct like Scheme's
@racket[cond]. @racket[cond] takes an arbitrary number of clauses of
the form @racket[(lhs rhs)], executing the first right-hand side whose
left-hand side is true, or executing a final @racket[else] clause if no
left-hand side is true.  This construct essentially expands into a
sequence of if/else expressions. However, if the programmer knows that
each clause is mutually exclusive, it is beneficial to sort the clauses
from most to least likely to succeed. In general, the compiler
cannot prove such a property and must emit the clauses in the original
order, so even traditional profile directed optimization cannot optimize
@racket[cond].


This paper presents a system for doing profile directed
meta-programming, including a workflow for using it with traditional low
level profile directed optimizations.  @Secref{api} presents the API,
@secref{examples} presents several examples of Scheme macros that use
this system, and @secref{implementation} discuses how this profiling
system is implemented.  

@;and @secref{results} presents some benchmark results @todo{maybe}.

While Scheme is used in this paper, the same techniques should work in
any language with sufficient meta-programming capabilities, such as
Template Haskell, C++ Templates, or MacroML. 
