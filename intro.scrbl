@section[#:tag "intro" "Introduction"]
Profile directed optimizations can enable better optimization by using
potentially more accurate run-time data than traditional heuristics.
Current techniques for doing profile directed optimization use profile
data gathered at the level of procedures or basic-blocks @todo{cite}. At
this low-level, much information about the program is lost, limiting
which optimizations the compiler can do. By bringing profile directed
optimizations up to the meta-programming level, programmers can use
specific knowledge of the problem domain, program, and the high-level
code to optimize programs in ways a compiler can not. 

A simple example is a conditional branching construct like Scheme's
@racket{cond}. @racket{cond} takes an arbitrary number of clauses of
the form @racket{(lhs rhs)}, executing the first right-hand side whose
left-hand side is true, or executing a final @racket{else} clause if no
left-hand side is true.  This construct essentially expands into a
sequence of if/else expressions. However, if the programming knows that
each clause is mutually exclusive, it is beneficial to sort the clauses
from most likely to least likely to succeed. In general, the compiler
cannot prove such a property and must emit the clauses in the original
order, so even a compiler with traditional profile direct optimization
cannot help.

This paper presents a system for doing profile directed meta-programming
that can work with block-level profile direct optimizations.
Section~@secref{api} presents the API, section~@secref{examples}
presents several examples of Scheme macros that use this system and how
they work with block-level profiling, section~@secref{implementation}
discuses how this profiling system is implemented, and
section~@secref{results} presents some benchmark results @todo{maybe}.

While Scheme is used in this paper, the technique should work in any
language with sufficient meta-programming capabilities, such as Template
Haskell, C++ Templates, and MacroML. 
