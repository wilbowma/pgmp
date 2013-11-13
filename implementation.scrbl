#lang scribble/base
@(require "defs.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "implementation" "Implementation"]
This section describes our implementation of the profiling system, and
how source-level and block-level profile directed optimizations can work
together in our system. First we present how code is instrumented to
collect profile information. Then we present how profile information is
stored and accessed. Finally we present how we use both source-level and
block-level profile directed optimizations in the same system. 

@section{Source + block profiling}
@todo{Move this}
When designing our source level profiling system, we aimed to take
advantage of prior work on low level profile directed optimizations
@todo{cite}. However, optimizations based on source-level profile
information may result in a different set of blocks than the blocks
generated for the profiled run of a program. If blocks are profiled
for instance, by assigning each block a number in the order in
which the blocks are generated, then the block numbers will not be
consistent after optimizing with source information. Therefore
optimization using source profile information and those using block
profile information cannot be done after a single profiled run of a
program.

We take the @todo{Not naive} naive approach to block profiling and use the following
workflow to take advantage of both source and block leve profile
directed optimizations. First we compile and instrument a program to
collect source-level information. We run this program and collect only
source-level information. Next we recompile and optimize the program
using the source-level information only, and instrument the program to
collect block-level information. The profile directed meta-programs
reoptimize at this point.  We run this program and collect only the
block-level information.  Finally, we recompile the program with both
source-level and block-level information. Since the source information
has not changed, the meta-programs generate the same source code, and
thus the compiler generates the same blocks. The blocks are then
optimized with the correct profile information.

While the workflow seems to significantly complicate the compilation
process, the different between using only block-level profiling
and using both source-level and block-level profiling is small. To use
any kind of profile directed optimizations requires a 300% increase in
the number of steps (from compile to compile-profile-compile). To use
both source-level and block-level profile directed optimizations
requires only an additional 66% increase in number of steps
(compile-profile-compile to compile-profile-compile-profile-compile).


@todo{Fix up, reorganize}
We represent profile information as a floating point number between 0
and 1. Profile information is not stored as exact counts, but as
execution frequency with respect to the most executed expression
(refered to as `percent of max'). If an expression @racket[e1] is
executed 1 time, and the most frequently executed expression
@racket[e10] is executed 10 times, then @racket[(profile-query-weight
e1)] returns .1, while @racket[(profile-query-weight e10)] returns 1. 


@section{Instrumenting code}
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
never be thrown out or duplicated, even if @racket[e] is. 
@todo{Make mention of how this affects pattern-matching optimizations,
i.e. a compiler that uses nanopass.}
@todo{Mention how profile info can be used for coverage checking?}

These profile forms are retained until basic blocks are generated. While
generating basic blocks, the source objects from the profile forms are
gathered up and attached to the basic block in which they appear. When a
basic-block is entered, every instruction in that block will be
executed, so any profile counters in the block must be incremented.
Since all the profile counters must be incremented, it is safe to
increment them all at the top of the block. 

In our implementation, we attempt to minimize the number of counters
executed at runtime. After generating basic blocks and attaching the
source objects to their blocks, we analyze the blocks to determine which
counters can be calculated in terms of other counters. If possible, a
counter is computed as the sum of a list of counters (+counters)
minus the sum of a list of counters (-counters). This complicated the
internal representation of counters and the generation of counters, but
decreases the overhead of profiling.
@todo{This explanation is probably wrong}

To instrument block-level profiling, we reuse the above infrastructure
by creating fake source objects. When a file is compiled, we reset
global initial block number to 0, and create a fake source file
descriptor based on the file name. When creating blocks, each block is
given a source object using the fake file descriptor, and using the
blocks number as the starting and ending file position. This fake source
object is used when block-level profiling is enable. This fake source is
ignored and the list of sources from the source code is used when
source-level profiling is enable.
@todo{Maybe an example of creating fake sources}

@section{Storing and Loading profile data}
We store profile data by creating a hash table from source file names to
hash tables. Each second level hash table maps the starting file position
of the expression to the weighted count of the expression. This lookup
table is only populated after loading profile data from a file and not
from a current profiled run.  After loading profile data, it is
accessible through @racket[profile-query-weight]. 

Profile data is not immediately loaded into the lookup table after a
profiled run of a program. Profile data must first be dumped via
@racket[profile-dump-data] and then loaded via
@racket[profile-load-data]. 

To dump profile data, the run time gathers up all profile counters.
Recall that some counters are computed indirectly in terms of other
counters. The values for these indirect counters are computed. These
values with their associated source objects are then written to a file.
@todo{I'm not 100% sure about how this works and I need to be. Some of
the racket peoples were asking.}

To support loading multiple data sets, we do not load execution counts
directly into the lookup table. Instead we compute the percent of max
for each counter. Before loading a new data set, we find the maximum
counter value.  Each weighted count is computed as a percent of the
maximum counter value. If an entry for a source already exists in the
lookup table then we compute the weighted average of the previous entry
and the counter we're currently loading. We store the weighted count and
the current weight in the lookup table, incrementing the weight by one
with each new data set.
