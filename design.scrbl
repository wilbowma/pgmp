#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "design/implementation" "Design and Implementation"]
@todo{essential pieces sounds weird. "the design requirements of our approach?" "design of our approach?"
...}
This section presents the essential pieces of our approach, design
decisions, and implementation details.
We first discuss what profile information we use and how we handle
multiple data sets.
We then discuss source objects, which are used to identify and
expressions and track profile information.
We discuss how we efficiently instrument code, and finally we discuss
how we ensure source-level and block-level profile-guided optimizations
work together in our approach.

@;In a typical meta-programming situation, a meta-program takes as input
@;a @emph{source program} in a high-level domain-specific language (DSL)
@;and produces a @emph{target program} in some other language, e.g., C,
@;Haskell, or Scheme.
@;To perform arbitrary meta-program optimizations, we might require profile
@;information for arbitrary points in the source program, arbitrary points
@;in the target program, or both.

@section[#:tag "design-profile-weights"]{Profile Information}
In our implementations, we use counter-based profiling.
We associate unique counters with each profile point identified by the
meta-program.
Our approach is not specific to counter-based profiling and should work
just the same with, e.g., timing-based profiling.

While we track exact counts, exact counts are not comparable across
different data sets.
Multiple data sets are important to ensure PGOs can optimize for
multiple classes of inputs expected in production.
Instead, our API provides profile @emph{weights}.
The profile weight of a source point in a given data set is the ratio of
the exact count for the source point to the maximum count for any source
point, represented as a number in the range [0,1].
That is, the profile weight for a given source object is profile count
for that source object divided by the the profile count for the most
frequently executed source object in the database.
This provides a single value identifying the relative importance of an
expression and simplifies the combination of multiple profile data sets.

@;We also considered using ratios of individual counts to total or
@;average counts.
@;In both cases, the results are distorted when there are a few heavily
@;executed expressions, potentially leading to difficulty distinguishing
@;profile weights for two less frequently executed expressions.
@;We also considered using fixed-precision rather than floating point,
@;but the floating-point representation makes it easy to determine
@;the importance of a particular expression overall while still
@;providing substantial precision when comparing the counts for source
@;points with similar importance.

To understand how we compute profile weights, consider our running
example from @figure-ref{if-r-eg}. Suppose in our first benchmark,
@racket[(flag email 'important)] is executed 5 times and @racket[(flag
email 'spam)] is executed 10 times. In a second benchmark, @racket[(flag
email 'important)] is executed 100 times and @racket[(flag email 'spam)]
is executed 10 times. @Figure-ref{profile-weight-comps} shows the
profile weights computed after each benchmark.
@figure-here["profile-weight-comps" "Sample profile weight computations"
@#reader scribble/comment-reader
@codeblock0|{
;; After loading data from benchmark 1
(flag email 'important)→ 5/10             ;; 0.5
(flag email 'spam)     → 10/10            ;; 1

;; After loading data from benchmarks 1 and 2
(flag email 'important)→ (.5 + 100/100)/2 ;; 0.75
(flag email 'spam)     → (1 + 10/100)/2   ;; 0.55
}|]

@section[#:tag "design-source-obj"]{Source objects}
We use @emph{source objects}@~cite[dybvig93] to uniquely identify
points in a program to profile.
Source objects are typically introduced by the lexer and parser for a
source language and maintained throughout the compiler to correlate
source expressions with intermediate or object code. This enables both
error messages and debuggers to refer to source expressions instead of
target or intermediate representations.

We reuse source objects in our approach to uniquely identify profile
counters. If two expressions are associated with the same source
object, then they both increment the same profile counter when executed.
Conversely, if two expressions are associated with different source
objects, then they increment different profile counters when executed.

While source objects are typically introduced by the lexer and parser,
we also require the ability to create new source objects in
meta-programs.
This is useful, for instance, when implementing a DSL. You may want to
profile generated expressions separately from any other expression in
the source language.

In the case of our running example, the lexer and parser introduce
source objects for each expression (and subexpression). That is,
separate source objects are created for @racket[#'(if ...)],
@racket[#'(subject-contains-ci "PLDI")], @racket[#'subject-contains-ci],
@racket[#'"PLDI"], @racket[#'(flag email 'spam)], and so on. Note that
@racket[#'flag] and @racket[#'email] appear twice, and will have a
unique source object for each occurance.

@subsection{Chez Scheme Source Objects}

In Chez Scheme, a source object contains a file name and
starting and ending character positions. The Chez Scheme reader
automatically creates and attaches these to each piece of syntax read
from a file.

Chez Scheme also provides an API to programmatically
manipulate source objects and attach them to syntax@~cite[csug-ch11].
We use the function defined in
@figure-ref{make-source-obj-chez} to generate new source objects.  This
function takes a string prefix and creates a new source object
generator. The generator uses the prefix, a counter, and some given
piece of syntax to generate a fake file name and fake character positions.
The generated file name is partly based on the input syntax to make them
more useful if when they show up in error messages.
@figure["make-source-obj-chez" "Chez implementation for generating source objects"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
@(RACKETBLOCK0
(define (make-fresh-source-obj-factory! prefix)
    (let ([n 0])
      (lambda (syn)
        (let* ([sfd (make-source-file-descriptor
                      (format "~a:~a:~a" (syntax->filename syn) prefix n) #f)]
               [src (make-source-object n n sfd)])
          (set! n (add1 n))
          src)))))]

@subsection{Racket Source Objects}

Racket does not attach separate source objects to syntax.
Instead, the file name, line number, column number, position, and span
are all attached directly to the syntax object. We provide wrappers to
extract these into separate source objects, called @racket[srcloc]s, and to merge these source
objects into Racket syntax objects.
@Figure-ref{make-source-obj-racket} shows the Racket implementation
of the previous function. Again we generate a fake file names, but simply
copy the line number, column number, etc. from the given syntax object.
@figure["make-source-obj-racket" "Racket implementation for generating source objects"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
@(RACKETBLOCK0
(define (make-fresh-source-obj-factory! prefix)
  (let ([n 0])
    (lambda (syn)
      (let ([src (struct-copy srcloc (syntax->srcloc syn)
                   [source (format "~a:~a:~a" (syntax-source syn) prefix n)])])
        (set! n (add1 n))
        src)))))]

@section[#:tag "impl-instrumenting"]{Instrumenting code}
In this section we discuss how we instrument code to collect profiling
information.

@subsection{Chez Scheme Instrumentation}

The naïve method for instrumenting code to collect source profile
information would be to add a counter for each source expression.
However this method can easily distort the profile counts. As expressions are
duplicated or thrown out during optimizations, the source information is
also duplicated or lost.

Instead we create a separate profile form that is created after macro
expansion. Each expression @racket[_e] that has a source object
attached is expanded internally to @racket[(begin (profile _src) _e)],
where @racket[_src] is the source object attached to @racket[_e]. The
profile form is considered an effectful expression and should
never be thrown out or duplicated, even if @racket[_e] is. While the
separate profile form has benefits, it can interfere with optimizations
based on pattern-matching on the structure of expressions, such as those
implemented in a nanopass framework@~citea{keep2013nanopass}.

We keep profile forms until generating basic blocks. While
generating basic blocks, all the source objects from the profile forms are
attached to the basic block in which they appear. When a
basic block is entered, every instruction in that block will be
executed, so it is safe to increment the counters for all
the instructions in the block at the top of the block.

In our implementation, we minimize the number of counters
incremented at runtime. After generating basic blocks and attaching the
counters to blocks, we analyze the blocks to determine which
counters can be calculated in terms of other counters. If possible, a
counter is computed as the sum of a list of other counters.
This complicates the internal representation of counters and the
generation of counters, but decreases the overhead of profiling. These
techniques are based on the work of Burger and Dybvig@~cite[burger98].
We generate at most one increment per block, and fewer in practice.

To instrument block-level profiling, we reuse the above infrastructure
by creating fake source objects. Before compiling a file, we reset
global initial block number to 0, and create a fake source file
based on the filename. We give each block a source object using the
fake filename and using the blocks number as the starting and ending
file position.

When loading profile information from a previous run, we compute profile
weights and store them in a two-level hash table.  The first level hash
table maps source file names to hash tables. Each second level hash
table maps the starting character position to a profile weight. These
tables are not updated in real time, only when a new data set is
manually loaded by an API call in a program or meta-program.

After the hash tables are populated, the information can be accessed via
the function @racket[profile-query-weight].
The function @racket[profile-query-weight] takes a source object or
syntax object.
When given a syntax object, it extract the source object first.
The result of @racket[profile-query-weight] is either the profile weight
associated with the given source object, or @racket[#f] (the Scheme
value for false) if no profile information exists.
In our running example, we called this @racket[profile-query], and
ignored the possibility of get @racket[#f], to simply the example.

@subsection{Racket Instrumentation}

We use one of the pre-existing Racket profiling systems. The
@racket[errortrace] library provides exact profile counters, like the
Chez Scheme profiler. We implement several wrappers to provide an API
similar to the API provided by Chez Scheme. All these wrappers are
implemented simply as Racket functions that can be called at compile
time, requiring no change to either the Racket implementation
or the @racket[errortrace] library.

@todo{It really ought to use hash tables. Current implementation is
kinda dumb.}
The current Racket implementation does not use hash tables the way the
Chez Scheme implementation does. Profile information is simply stored as an
association list mapping source objects to profile counts. Profile
weights are computed on each call to @racket[profile-query-weight].

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
@;todo{I'm not 100% sure about how this works and I need to be. Some of
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

@section[#:tag "impl-source-block"]{Source and block PGO}
In this section we describe how our approach works with both source and
block-level PGO.
This section is only relevant to our Chez Scheme implementation.
While the Chez Scheme compiler produces machine code and offers
block-level PGOs, Racket uses a JIT compiler and does not take profile
information as input.
@todo{Have a Racketeer double check that last sentence. And may rephrase
it}

When designing our source level profiling system, we wanted to continue
using prior work on low level profile-guided optimizations
@~citea["hwu89" "pettis90" "gupta02"].
However, optimizations based on source-level profile information may
result in a different set of blocks.
If we use the same profile information gathered when profiling the
source, the block-level profile information will be stale.
Therefore optimization using source profile information and those using
block profile information cannot be done after a single profiled run of
a program.
We need a new workflow.

To use both source and block-level PGO, first we compile
and instrument a program to collect source-level information.
We run this program and collect only source-level information.
Next we recompile and optimize the program using the source-level
information only, and instrument the program to collect block-level
information.
From this point on, source-level optimizations should run
and the blocks should remain stable.
We run this program and collect only the block-level information.
Finally, we recompile the program
with both source-level and block-level information.
Since the source information has not changed, the meta-programs generate
the same source code, and thus the compiler generates the same blocks.
The blocks are then optimized with the correct profile information.

For example, to get both source-level and block-level optimizations on
our running example, we would first instrument the program in
@figure-ref{if-r-eg} for source profiling.
After running it on representative inputs, we get the profile weights
such as in @figure-ref{profile-weight-comps}.
Next we recompile using that source profile information, and instrument
block profiling.
The resulting source program is the code from @figure-ref{if-r-expand}.
Since we will continue to use the same source profile information,
@racket[if-r] continues to expand to the same source code, regardless of
the block profile information, so the blocks remain stable.
Now we pass the optimized source program and block profile information to
the compiler to perform block-level optimizations.
