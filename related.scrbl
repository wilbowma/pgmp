#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure)

@todo{Add links for all references.}

@title[#:tag "related" "Related Work"]
@section{Profile-guided optimizations}
Modern compilers such as GCC, .NET, and LLVM@~cite[lattner02] use
profile-guided optimizations.
These systems use profile information to guide decisions about code
positioning, register allocation, inlining, and conditional branch
ordering.

GCC profiles at the level of an internal control-flow graph (CFG).
To maintain a consistent CFGs across instrumented and optimized
builds, GCC requires similar optimization decisions across
builds@~citea{chen10}.
In addition to the common optimizations noted previously, .NET extends
their profiling system to probe values in @code{switch}
statements.
They can use this value information to reorder the branches of a
@code{switch} statement, as done for @racket[case] in @secref{study-case}.

LLVM has a different model for PGO.
LLVM uses a runtime reoptimizer that monitors the running
program.
The runtime can profile the program as it runs ``in the field'' and
perform simple optimizations to the machine code, or call to an offline
optimizer for more complex optimizations on the LLVM bytecode@~citea{lattner2004llvm}.

Recent work is still finding novel uses for profile information.
@citeta{furr09} use profile information to infer types in dynamic
languages to assist in debugging.
@citeta{chen06:garbage} use profile information to reorganize the heap and optimize
garbage collection.
@citeta{luk02} use profile information to guide data prefetching.
@citeta{debray02} use profile information to compress infrequently
executed code on memory constrained systems.

@section{Meta-program optimizations}
Meta-programming combines the ability to provide high-levels of
abstraction while producing efficient code.
Meta-programming has been widely used to implement high performance
domain-specific languages
@~citea["sujeeth13" "sujeeth2014delite" "rompf10"], whole
general purpose languages@~citea["rafkind12" "tobin-hochstadt08"
"barzilay05"],
and production-quality compiler generators@~citea["keep2013nanopass"].
@citeta{tobin-hochstadt11} implement the optimizer for the Typed Racket
language as a meta-program.
The HERMIT toolkit provides an API for performing program
transformations on Haskell intermediate code before compiling, even
allowing interactive experimentation@~citea["farmer2012hermit"].
@citeta["hawkins11" "hawkins12"] implement a compiler for a language that generates
C++ implementations of data structures based on high-level
specifications.

Previous work integrates profiling to guide meta-program optimizations.
@citeta{chen06:mpipp} use profile-guided meta-programming for performing process
placement for SMP clusters.
@citeta{vsimunic2000source} use profile-guided meta-programming to
optimize the energy usage of embedded programs.
@citeta{karuri2005fine} use fine-grained source profile information to
optimize ASIP designs.

However, existing work introduces new toolkits for profiling and
meta-programming.
We provide a single, general-purpose approach in which we can implement
new general-purpose languages, domain-specific languages abstract
libraries, and arbitrary meta-programs,
all taking advantage of profile-guided optimizations.
Further, our approach reuses existing meta-programming and profiling
facilities, rather than implementing new tools that interface the
compiler in ad-hoc ways.

@;@section{Dynamic Recompilation}
@;The standard model for PGO requires the instrument-profile-optimize
@;workflow.
@;While not currently enabled, our mechanism supports this kind of
@;reoptimization. We build on the work of Burger and Dybvig, who present
@;an infrastructure for profile-directed dynamic
@;reoptimization@~cite[burger98]. Their work shows just 14% run-time
@;overhead for instrumented code, but they express concerns that dynamic
@;recompilation will not overcome this cost. Our internal microbenchmarks show
@;similar overhead. To enable dynamic PGO, we would need to modify our
@;mechanism to automatically reload profile information, such as whenever
@;@racket[profile-query-weight] is called, instead of manually loading
@;information from a file. This is a trivial change to our system, but we
@;have no optimizations in mind that make use of profile-guided at
@;runtime. It may also increase overhead, since we compute profile
@;weights and many counters when loading new profile data.

@; PDO has some limitations. If profile data is inaccurate it can slow down
@; a program by optimizing for the wrong cases. This can happen when
@; programs are profiled on a set of benchmarks that don't represent the
@; real-world parameters of a program. The need to compile twice and run
@; the program leads to a longer development process. These limitations
@; make developers unlikely to even use PDOs @todo{cite 2, 7}. we
@; find

@;http://dl.acm.org/citation.cfm?id=582432
