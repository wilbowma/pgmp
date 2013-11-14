#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "implementation" "Implementation"]
This section describes details of how profile
information is represented, how code is instrumented, and how
source-level and block-level profile directed optimizations can work
together in our system. First we present how we represent source objects
and profile information. Next we describe how code is instrumented to
collect profile information. Then we present how profile information is
stored and accessed. Finally we present how we use both source-level and
block-level profile directed optimizations in the same system. 

@section{Source objects}
In the previous sections we elided what exact a source object is,
assuming that we can use them as keys, create fresh ones, and attach
them to syntax. Chez Scheme implements source objects to use in
error messages. A source object contains a filename, line number, and
starting and ending character positions. The Chez Scheme reader
automatically creates and attaches these to each piece of syntax read
from a file, but Chez Scheme also provides an API to programmatically
manipulate source objects. This is useful when using Chez Scheme as
a target language. Custom source objects can be attached to
target syntax to provide error messages with line and character
positions in the source language@~cite[csug-ch11].

To create custom source objects for for fresh profile counter, we can
use arbitrary filenames, lines numbers, and character positions. For
instance, in @secref{eg-datatype} we create custom source objects to
profile list and vector operations. In our implementation, these might
be created as seen in @figure-ref{really-make-source}.

@figure["really-make-source" "Creating custom source objects"
@#reader scribble/COMMENT-READER-T
@(RACKETBLOCK
...
(define list-src (make-source-object "sequence-generate-src" 0 0 0))
(define vector-src (make-source-object "sequence-generate-src" 1 0 0))
...)]

@section{Profile weights}
We represent profile information as a floating point number between 0
and 1. As mentioned in @secref{design}, profile information is not
stored as exact counts, but as a weighted relative count. We considered
using Scheme fixnums (integers) for additional speed, but fixnums
quickly loose precision, particularly when working with multiple data
sets.

We store profile weights by creating a hash table from source file names to
hash tables. Each second level hash table maps the starting file position
to a profile weight. These tables are not updated in real time, only
when a new data set is manually loaded via @racket[profile-load-data].

@section{Instrumenting code}
The naive method for instrumenting code to collect source profile
information is to attach the source information to each AST node
internally. At an appropriately low level, that source information can
be used to generate code that increments profile counters. However this
method can easily distort the profile counts. As nodes are duplicated or
thrown out during optimizations, the source information is also
duplicated or lost.

Instead we create a separate profile form that is created after macro
expansion. Each expression @racket[e] that has source information
attached is expanded internally to @racket[(begin (profile src) e)],
where @racket[src] is the source object attached to @racket[e]. The
profile form is consider an effectful expression internally and should
never be thrown out or duplicated, even if @racket[e] is. This has the
side-effect of allowing profile information to be used for checking
code-coverage of test suites. While the separate profile form has
benefits, it can interfere with optimizations based on pattern-matching
on the structure of expressions, such as those implemented in a nanopass
framework@~citea{keep2013nanopass}. 

We keep profile forms until generating basic blocks. While
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
counter is computed as the sum of a list of other counters.
This complicated the internal representation of counters and the
generation of counters, but decreases the overhead of profiling. These
techniques are based on the work of Burger and Dybvig@~cite[burger98]

To instrument block-level profiling, we reuse the above infrastructure
by creating fake source objects. Before compiling a file, we reset
global initial block number to 0, and create a fake source file
based on the file name. We give each block a source object using the
fake file name and using the blocks number as the starting and ending
file position. 

@;@section{Storing and Loading profile data}
@;We store profile data by creating a hash table from source file names to
@;hash tables. Each second level hash table maps the starting file position
@;of the expression to the weighted count of the expression. This lookup
@;table is only populated after loading profile data from a file and not
@;from a current profiled run.  After loading profile data, it is
@;accessible through @racket[profile-query-weight]. 
@;
@;Profile data is not immediately loaded into the lookup table after a
@;profiled run of a program. Profile data must first be dumped via
@;@racket[profile-dump-data] and then loaded via
@;@racket[profile-load-data]. 
@;
@;To dump profile data, the run time gathers up all profile counters.
@;Recall that some counters are computed indirectly in terms of other
@;counters. The values for these indirect counters are computed. These
@;values with their associated source objects are then written to a file.
@;@todo{I'm not 100% sure about how this works and I need to be. Some of
@;the racket peoples were asking.}
@;
@;To support loading multiple data sets, we do not load execution counts
@;directly into the lookup table. Instead we compute the percent of max
@;for each counter. Before loading a new data set, we find the maximum
@;counter value.  Each weighted count is computed as a percent of the
@;maximum counter value. If an entry for a source already exists in the
@;lookup table then we compute the weighted average of the previous entry
@;and the counter we're currently loading. We store the weighted count and
@;the current weight in the lookup table, incrementing the weight by one
@;with each new data set.

@section{Source and block PGO}
When designing our source level profiling system, we aimed to take
advantage of prior work on low level profile-guided optimizations
@~citea["hwu89" "pettis90"]. However, optimizations based on
source-level profile information may result in a different set of
blocks, so the block-level profile information will be stale. Therefore
optimization using source profile information and those using block
profile information cannot be done after a single profiled run of a
program.

To take advantage of both source and block-level PGO, first we compile
and instrument a program to collect source-level information. We run
this program and collect only source-level information. Next we
recompile and optimize the program using the source-level information
only, and instrument the program to collect block-level information. The
profile directed meta-programs reoptimize at this point.  We run this
program and collect only the block-level information.  Finally, we
recompile the program with both source-level and block-level
information. Since the source information has not changed, the
meta-programs generate the same source code, and thus the compiler
generates the same blocks. The blocks are then optimized with the
correct profile information.
