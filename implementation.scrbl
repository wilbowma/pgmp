#lang scribble/base
@(require "defs.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@section[#:tag "implementation" "Implementation"]
This section describes our implementation of the profiling system, and
how source-level and block-level profile directed optimizations can work
together in our system. First we present how code is instrumented to
collect profile information. Next we present how we handle loading data
from multiple profile runs. Then we present how profile information is
stored and access. Finally we present how we use both source-level and
block-level profile directed optimizations in the same system. 

@todo{Definitely going to need Kent to check this section.}

@subsection{Instrumenting code}
The naive method for instrumenting code to collect source profile
information is to attach the source information to each AST node
internally. At an appropriately low level, that source information can
be used to generate code that increments profile counters. However this
method can easily distort the profile counts. As nodes are duplicated or
thrown out during optimizations, the source information is also
duplicated or lost.

Instead we create a separate profile form that is created during macro
expansion. Each expression @racket[e] that has source information
attached is expanded internally to @racket[(begin (profile src) e)],
where @racket[src] is the source object attached to @racket[e]. The
profile form is consider an effectful expression internally and should
never be thrown out or duplicated, even if @racket[e] is. This has the
side-effect of allows profile information to be used to check for code
coverage in a profiled run. 
@todo{Make mention of how this affects pattern-matching optimizations,
i.e. a compiler that uses nanopass.}
@todo{That last sentence is awful. and a little out of place}

These profile forms are retained until basic blocks are generated. While
generating basic blocks the source objects from the profile forms are
gathered up and attached to the basic block in which they appear. Since
each of these profile forms will be `executed' if the basic block is
executed, it is safe to `execute' them all at the top of the block. More
interestingly, we can use this knowledge to reduce how many counters
must be incremented.

In our implementation we attempt to minimize the number of counters
executed at runtime. After generating basic blocks and attaching the
source objects to their blocks, we analyze the blocks to determine which
counters can be calculated in terms of other counters. If possible, a
counter is computed as the sum of a list of counters (+counters)
minus the sum of a list of counters (-counters). This complicated the
internal representation of counters and generating of counters, but
decreasing the overhead of profiling.
@todo{This explanation is probably wrong}

To add instrument block-level profiling, we reuse the above
infrastructure by creating fake source objects. When a file is compiled,
we reset global initial block number to 0, and create a fake source file
descriptor based on the file name. When creating blocks, each block is
given a source object using the fake file descriptor, and using the
blocks number as the starting and ending file position. This fake source
object is used when block-level profiling is enable.
@todo{Maybe an example of creating fake sources}

@subsection{Loading profile data}
@subsection{Accessing profile data}

@subsection{Source + block profiling}
@todo{Not sure where this subsection belongs}
Optimizations based on source-level profile information may result in a
different set of blocks than the blocks generated on a previous run of a
program. If blocks are profiled naively, for instance, by assigning each
block a number in the order in which the blocks are generated, then the
block numbers will not be consistent after optimizing with source
information. Therefore optimization using source profile information and
those using block profile information cannot be done after a single
profiled run of a program.

To take advantage of both kinds of optimizations, we use the following
workflow. First a program is compiled and instrumented to collect
source-level information. A profiled run collects only the source-level
information. The program is recompiled and optimized using that
source-level information, and instrumented to collect block-level
information. A profiled run collects only the block-level information.
The program is finally recompiled and optimized using both the
source-level information and the block-level information.

While the workflow seems to significantly complicate the compilation
process, the different between using only block-level profiling
and using both source-level and block-level profiling is small. To use
any kind of profile directed optimizations requires a 300% increase in
the number of steps (from compile to compile-profile-compile). To use
both source-level and block-level profile directed optimizations
requires only an additional 66% increase in number of steps
(compile-profile-compile to compile-profile-compile-profile-compile).
