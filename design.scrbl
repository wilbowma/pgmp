#lang scribble/base
@(require "defs.rkt")
@title[#:tag "design" "Design"]
@(require scribble/manual)
This section presents the design of our profile system. We discuss the
system at a high-level and sketch implementations for other
meta-programming systems. We discuss implementation details in
@secref{implementation}

@section{Source and syntax objcets}
In Scheme, macros (Scheme meta-programs) operate on @emph{syntax
objects}, direct representations of Scheme syntax, and can run arbitrary
Scheme code.  Each syntax object has an associated @emph{source
object}---a filename and a beginning and ending character position for
the expression. To abstract away from the particular syntax, we
associate profile information with these source objects. To access
profile information, all we require is a function that allows retrieving
profile information from a syntax or source object. We added the
function @racket[profile-query-weight] to our Scheme implementation.
Given a syntax object, @racket[profile-query-weight] returns a number
between 0 and 1, or false if there is no profile information associate
with that piece of syntax. 

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

We considered comparing to the total number of expressions executed and
the average number of times an expression is executed.  In both cases,
the results are distored when there are a large number of expressions
that are executed infrequently. In that case, a main loop might look
infrequently executed if there are many start up or shut down steps. By
comparing to the most expensive expression, we have a relatively stable
comparison of how expensive some expression is, even in cases with many
unused expressions or a few very expensive expressions.

This relative information is not perfect. Loop unrolling can benefit
from exact counts more than a weight. If we know a loop is executed
exactly 5 times, unrolling it 5 times might make sense. If we know a
loop is executed 20% of the max, we do not know if the loop is executed
1 or 1,000,000 times. 

Ideally we would track both relative and exact information, but this
doubles profiling overhead. One of our design goals is to enable
`always on' profiling, so even release builds of software can have
profiling enabled without too much performance impact. Using previous
work to decrease profiling overhead @todo{cite}, running a set of
benchmarks with profiling enables gives only 10% slowdown @todo{Make
these reproducable}.

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

@section{Comparison to other meta-programming languages}
We take advantage of Scheme's powerful meta-programming facilities that
allows running full-fledged Scheme programs at compile time.  While many
programming langauges have meta-programming systems, their
expressiveness and support for manipulating syntax varies.

@todo{figure out if (profile-query-weight ) could be run in C++,
MetaOCaml at compile time}

@para{C++} limits inspecting syntax and does not allow IO at
compile-time. Without compile-time IO, it seem that template
meta-programming in C++ cannot currently support profile directed
optimization through meta-programming. 

@para{Template Haskell} @todo{cite} supports generating Haskell code via
quotes and splicing, similar to Scheme and MetaOCaml, but also provides
constructors to create Haskell ASTs directly. Template Haskell allows IO
and running ordinary Haskell functions at compile-time. This suggests 
compilers DSLs written in Haskell can easily incorporate a source
expression profiler and allow profile directed optimizations at compile
time. Because Template Haskell can manipulate Haskell syntax, it should
be simple to even write an optimization for Haskell in Template Haskell. 

@para{MetaOCaml} @todo{cite} supports generating OCaml code through
quotes and splicing, similar to Scheme and Template Haskell. MetaOCaml
allows IO and running ordinary OCaml functions at compile-time, however,
it discourages inspecting OCaml syntax. This suggest compilers for DSLs
written as OCaml data can easily incorporate a source expression
profiler and allow  profile directed optimizations at compile time.

@todo{Look at some other systems}
@todo{cite Czarnecki04} for more in-depth comparison of those three.
