#lang scribble/sigplan
@(require "defs.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@section[#:tag "related" "Related Work"]
@todo{I'm not sure what I'm doing with this section yet.}
Profile directed optimization is not a new concept. @todo{cite 1} presents
a compiler that uses profile information to direct code positioning,
reducing instruction cache misses and improving call locality.

Previous work finds that instrumenting code can be inaccurate and cause
substantial overhead @todo{cite 2}. However, our techniques for reducing
the number of counters and our careful representation of profile forms
allows accurate source profiling with little overhead @todo{measure
overhead on a standard set of benchmarks. The benchmarks I ran at cisco
suggest ~10% overhead, but those are not publically accessible}

Modern systems such as GCC, .NET, and LLVM use profile directed
optimizations. @todo{cite 5, 4, 6}However, these systems provide mostly low level (i.e.
block level) optimizations, such as optimizations for block order,
register allocation, and dead code. 

.NET provides a couple higher level optimizations, such as function
inlining and conditional branch optimization similar to
@racket[exclusive-cond] and @racket[case] presented here. Their
technique requires value probes for switch statements and block level
optimization for if/else branches.

GCC provides "tree-level" profiling instead of block level profiling.

LLVM gathers profile information through its run-time optimizer and 
can gather execution at various levels of granularity, but all at the
level of the running machine code. 

@itemlist[
@item[@link["http://pages.cs.wisc.edu/~fischer/cs701.f06/code.positioning.pdf"
"1"]]
@item[@link["http://pdf.aminer.org/000/244/348/commercializing_profile_driven_optimization.pdf"
"2"]]
@item[@link["http://msdn.microsoft.com/en-us/library/aa289170.aspx" "3"]]
@item[@link["http://msdn.microsoft.com/en-us/library/e7k32f4k(v=vs.90).aspx"
"4"]]
@item[@link["http://www.llvm.org/pubs/2002-12-LattnerMSThesis-book.pdf"
"5"]]
@item[@link["http://gcc.gnu.org/gcc-4.1/changes.html" "6"]]
]

