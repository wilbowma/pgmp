#lang scribble/sigplan
@(require scribble/manual)
@(require "defs.rkt")
@title[#:tag "intro" "Introduction"]
@todo{Need to look at more related work before I can do this section justice}
@; NB: What is profile directed optimization?
Profile directed optimization is a standard compiler technique that uses
sample data gathered at run-time to recompile and further optimize a
program. If the sample data is representative of how the program is used
in practice then this data can be more accurate than static heuristics
and can lead to a more optimized program.

@; NB: What do current compilers/techniques allow?
Compilers such as .NET, GCC, and LLVM @todo{cite} do low level profile
directed optimization such as reordering blocks, unrolling loops, and
optimizing switch statements. These optimizations are often fragile
because of their dependence on low level, compiler internal structures.
Small changes to source code can imply large changes to low level
representations. By bringing profile directed optimizations up to the
source level, programmers can use specific knowledge of the problem
domain, program, and the high-level code to optimize programs in ways a
compiler can not. 

@; NB: Why is this not sufficient?

@todo{I need to more carfully read related work}

@; NB: Motivate: Why is this an advance? Who can benefit? (where do I
@; talk about DSLs)

@; To motivate why these low level optimizations are not enough, and
@; demonstrate our framework, we consider three problems that the writer of
@; a domain-specific language (DSL) or DSL library writer might want to
@; solve. First we consider the standard technique of loop unrolling and
@; demonstrate unrolling loops and general recursive functions based on
@; profile information. Next we consider the problem of inline caching for
@; a DSL with objects and demonstrate reordering clauses of a generic
@; branching construct based on profile information. Finally we consider
@; the EDSL library writer with users that don't understand enough about
@; data structures to pick the write collection, and demonstrate datatype
@; specialization based on profile information. 
@; 
@; 
@; First we consider the standard technique of loop unrolling and
@; demonstrate unrolling loops and general recursive functions based on
@; profile information. While loops can be unrolled using traditional low
@; level profile information, we show the fine granularity of source level
@; information makes this problem trivial.
@; 
@; Next we consider the problem of inline caching for a DSL with objects
@; and demonstrate reordering clauses of a generic branching construct
@; based on profile information.  
@; 
@; Finally we consider the EDSL library writer with users that don't
@; understand enough about data structures to pick the write collection,
@; and demonstrate datatype specialization based on profile information. 

A simple example is a conditional branching construct like Scheme's
@racket[cond]. @racket[cond] takes an arbitrary number of clauses of the
form @racket[(lhs rhs)], executing the first right-hand side whose
left-hand side is true, or executing a final @racket[else] clause if no
left-hand side is true.  This construct essentially expands into a
sequence of if/else expressions. However, if the programmer knows that
each clause is mutually exclusive, it is beneficial to sort the clauses
from most to least likely to succeed. In general, the compiler cannot
prove such a property and must emit the clauses in the original order,
so even traditional profile directed optimization cannot optimize
@racket[cond].
@todo{example, less about cond corner cases}

@; NB: How do we advance the state of the art?
This paper presents a system for doing profile directed
meta-programming, including a workflow for using it with traditional low
level profile directed optimizations.  @Secref{design} presents the
design of our system at a high level and how other macro systems could
use it, @secref{examples} presents several examples of Scheme macros
that use this system, and @secref{implementation} discuses how this
profiling system is implemented.  

@;and @secref{results} presents some benchmark results @todo{maybe}.

While Scheme is used in this paper, the same techniques should work in
any language with sufficient meta-programming capabilities, such as
Template Haskell, C++ Templates, or MacroML. 
