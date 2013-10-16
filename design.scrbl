#lang scribble/sigplan
@(require "defs.rkt")
@title[#:tag "design" "Design"]
@(require scribble/manual)
This section presents the design of our profile system. We discuss the
system at a high-level and sketch implementations for other macro
systems. We discuss implementation details in @secref{implementation}

@section{Source and syntax objcets}
In Scheme, macros operate on @emph{syntax objects}, and can run
arbitrary Scheme code. To access profile information, all we require is
a function that allows retrieving profile information from a syntax
object. We added the function @racket[profile-query-weight] to our
Scheme implementation. Given a syntax object,
@racket[profile-query-weight] returns a number between 0 and 1, or false
if there is no profile information associate with that piece of syntax. 

@section{Profile weight}
We represent profile information as a floating point number between 0
and 1. Profile information is not stored as exact counts, but as
execution frequency with respect to the most executed expression
(refered to as `percent of max'). If an expression @racket[e1] is
executed 1 time, and the most frequently executed expression
@racket[e10] is executed 10 times, then @racket[(profile-query-weight
e1)] returns .1, while @racket[(profile-query-weight e10)] returns 1. 

We use percent of max count in part because an exact execution count can
be meaningless in some contexts. Consider an expression that is executed
5 times. We cannot know if this statement is executed frequently or not
without some comparison. 

We choose percent of max because this seems to give the best relative
comparison. We considered comparing to the total number of expressions
executed and the average number of times an expression is executed.
In both cases, the results are distored when there are a large number of
expressions that are executed infrequently. In that case, a main loop
might look infrequently executed if there are many start up or shut down
steps. By comparing to the most expensive expression, we have a
relatively stable comparison of how expensive some expression is.

This relative information is not perfect. Loop unrolling can benefit
from exact counts more than a weight. If we know a loop is executed
exactly 5 times, unrolling it 5 times might make sense. If we know a
loop is executed 20% of the max, we do not know if the loop is executed
1 or 1,000,000 times.

@section{Source + block profiling}
When designing our source level profiling system, we aimed to take
advantage of prior work on low level profile directed optimizations
@todo{cite}. However, optimizations based on source-level profile
information may result in a different set of blocks than the blocks
generated for the profiled run of a program. If blocks are profiled
naively, for instance, by assigning each block a number in the order in
which the blocks are generated, then the block numbers will not be
consistent after optimizing with source information. Therefore
optimization using source profile information and those using block
profile information cannot be done after a single profiled run of a
program.

We take the naive approach to block profiling and use the following
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
