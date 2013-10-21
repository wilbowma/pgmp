#lang scribble/sigplan
@(require scribble/manual)
@(require "defs.rkt")
@(require "bib.rkt")
@title[#:tag "intro" "Introduction"]
@; Introduce meta-programming
Many languages, such as ML, C++, Haskell, Java, and Scheme, provide
powerful meta-programming facilities that help programmers create
generic libraries, new language constructs, and domain-specific
languages (DSLs)@~cite[taha00 erdweg11 czarnecki04 sheard02 dybvig93]
@todo{veldhuizen95}.
Meta-programming has been used to implement a number of DSLs and even
new languages@todo{felleisen04, tobin-hochstadt06, tobin-hochstadt11}.
Whole compilers and optimizers have been written as meta-programs
@todo{tobin-hochstadt11}. Unfortunately, meta-programmers do not have
all standard tools and techniques of traditional compiler writers, such
as profile directed optimizations.

@; NB: What is profile directed optimization?
Profile directed optimization is a standard compiler technique that uses
sample data gathered at run-time to recompile and further optimize a
program. If the sample data is representative of how the program is used
in practice then this data can be more accurate than static heuristics
and can lead to a more optimized program.

@; NB: What do current compilers/techniques allow?
Compilers such as .NET, GCC, and LLVM @todo{cite} profile low level
language representations such as control flow graphs or basic blocks
@todo{cite} and provide traditional profile directed optimizations such
as reordering blocks, inlining functions, unrolling loops, and
optimizing switch statements @todo{cite}. These traditional
optimizations can provide significant performance gains @todo{cite}, but
have been inaccessible to meta-programmers. This restricts how profile
information can be used, and limits DSL writers and meta-programmers.

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

@; NB: How do we advance the state of the art?
This paper presents a system for doing profile directed
meta-programming, including a workflow to use our techniques with
traditional low level profile directed optimizations.  We present this
system through several motivating examples in @secref{examples}. 
@Secref{design} presents the design of our system at a high level and
how other macro systems could use it, and @secref{implementation}
discuses how this profiling system is implemented.  @Secref{results}
presents some arbitrary numbers @todo{make numbers less arbitrary}.

@;and @secref{results} presents some benchmark results @todo{maybe}.

While Scheme is used in this paper, the same techniques should work in
any language with sufficient meta-programming capabilities, such as
Template Haskell, C++ Templates, or MacroML. 
