#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure)

@title[#:tag "related" "Related Work"]
@section{Profile-Guided Optimizations}
Contemporary compiler systems such as GCC, .NET, and LLVM@~cite[lattner02] use
profile-guided optimizations.
These systems use profile information to guide decisions about code
positioning, register allocation, inlining, and conditional branch
ordering.

@todo{The next four paragraphs seem kind of like disorganized thoughts}
GCC profiles at the level of an internal control-flow graph (CFG).
To maintain consistent CFGs across instrumented and optimized
builds, GCC requires similar optimization decisions across
builds@~citea{chen10}.
This requirement is similar to how we ensure consistency when using both
source and block-level PGOs in Chez Scheme.

In addition to the common optimizations noted previously, the .NET
profiler features special support for @code{switch} statements called
@emph{value probes}.
The .NET compilers use value probes to optimize @code{switch} statement
common values, similar to our optimization of @racket[case] expressions
in @Secref{study-case}.
Our design can express this optimization at the user level via the same profiler machinery
used in our other case studies.

LLVM takes a different approach to PGO.
LLVM uses a run-time reoptimizer that monitors the running program.
The run-time system can profile the program ``in the field''.
This run-time system can perform simple optimizations on the machine code
during program execution.
More complex optimization require running an offline optimizer on the
LLVM bytecode@~citea{lattner2004llvm}.
@citeta{burger1998infrastructure} develop a similar run-time recompilation mechanism
that allows simple optimizations to be performed at run time (during
garbage collection) but does not support source-level profile-guided
optimizations.

Recent work is still discovering novel applications for profile information.
@citeta{furr09} use profile information to infer types in dynamic
languages to assist in debugging.
@citeta{chen06:garbage} apply profile optimization to optimize garbage
collection.
@citeta{luk02} perform data prefetching guided by profile information.
@citeta{debray02} compress infrequently executed code based on profile
information.

@section{Meta-Program Optimizations}
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
@citeta["hawkins11" "hawkins12"] implement a compiler for a language
that generates C++ implementations of data structures based on
@nonbreaking{high-level specifications.}

Previous work also integrates profiling to guide meta-program
optimizations.
@citeta{chen06:mpipp} perform process placement for SMP clusters via
profile-guided meta-programming.
@citeta{vsimunic2000source} optimize source code using energy profiles,
although the bulk of the optimization requires programmer intervention.
@citeta{karuri2005fine} optimize ASIP designs with fine-grained source
@nonbreaking{profile information.}

In contrast, our own work introduces a single general-purpose approach
in which we can implement new general-purpose languages, domain-specific
languages, efficient abstract libraries, and arbitrary meta-programs,
all of which can take advantage of profile-guided optimizations.
Further, our approach reuses existing meta-programming and profiling
facilities rather than implementing new tools that interface with the
compiler in ad-hoc ways.
