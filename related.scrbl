#lang scribble/sigplan
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "related" "Related and Future Work"]
@todo{I'm not sure what I'm doing with this section yet.}
Modern systems such as GCC, .NET, and LLVM use profile directed
optimizations @~cite[lattner02 gcc .net]. However, these systems provide
mostly low level optimizations, such as optimizations for block order
and register allocation. In addition to limiting the kinds of
optimizations the compiler can do, this low-level profile information is
fragile.

GCC profiles an internal control-flow graph (CFG). To maintain a
consistent CFGs across instrumented and optimization builds, GCC
requires similar optimization decisions across builds. By associating
profile information with source expression we can more easily reuse
profile information @~cite[chen10].  In our system, all profile information for a source
file is usuable as long as the source file does not change.

.NET provides some higher level optimizations, such as function inlining
and conditional branch optimization similar to @racket[exclusive-cond]
and @racket[case] presented here. To optimize @racketkeywordfont{switch} statements,
.NET uses @emph{value} profiling in addition to execution count
profiling @~cite[.net]. By probing the values used in a switch statement,
the compiler can attempt to reorder the cases of the @racketkeywordfont{switch} 
statement. @todo{Value probes seem like a pretty ad-hoc method to get a
very specific optimization. I don't know if I want to say that.}

The standard model for profile directed optimizations requires the
instrument-profile-optimize workflow. LLVM has a different model for
profile directed optimization. LLVM uses a runtime reoptimizer that
monitors the running program. The runtime reoptimizer can profile the
program as it runs ``in the field'' and perform simple optimizations to
the machine code, or call off to an offline optimizer for more complex
optimiztions on the LLVM bytecode. 

Meta-programs generate code at compile time, so the examples presented
in @secref{examples} require the standard instrument-profile-optimize 
workflow. However, because we expose an API to access profiling
information, we could use this system to perform runtime decisions based
on profile information. To truly be beneficial, this requires keeping
the runtime overhead of profiling very low, which is not usually the
case @~cite[conte96 chen10]. However, our techniques for reducing
the number of counters and our careful representation of profile forms
allows accurate source profiling with little overhead @todo{measure
overhead on a standard set of benchmarks. The benchmarks I ran at cisco
suggest ~10% overhead, but those are not publically accessible. This
sentence belongs in implementation}. 

@; PDO has some limitations. If profile data is inaccurate it can slow down
@; a program by optimizing for the wrong cases. This can happen when
@; programs are profiled on a set of benchmarks that don't represent the
@; real-world parameters of a program. The need to compile twice and run
@; the program leads to a longer development process. These limitations
@; make developers unlikely to even use PDOs @todo{cite 2, 7}. we
@; find 

@;http://dl.acm.org/citation.cfm?id=582432
