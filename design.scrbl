#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "design"]{Approach and API}
This section presents the high-level design of our approach, discusses
design decisions, and presents a sketch of the API provided to
meta-programmers. We conclude section with a brief description of our
implementations of this approach in Chez Scheme and Racket.

@;In a typical meta-programming situation, a meta-program takes as input
@;a @emph{source program} in a high-level domain-specific language (DSL)
@;and produces a @emph{target program} in some other language, e.g., C,
@;Haskell, or Scheme.
@;To perform arbitrary meta-program optimizations, we might require profile
@;information for arbitrary points in the source program, arbitrary points
@;in the target program, or both.

@section[#:tag "design-profile-weights"]{Profile Information}
While our approach is not specific to a particular profile technique, we
refer to counter-based profile information to simplify explanations.
We consider that profile point identified by the meta-program has a
unique counter associated with it

Absolute profile information such as exact counts are difficult to
compare across different data sets.
Multiple data sets are important to ensure PGOs can optimize for
multiple classes of inputs expected in production.
Instead, our API provides profile @emph{weights}.
The profile weight of a profile point in a given data set is the ratio of
the absolute profile information for that profile point to the maximum of
all other profile point, represented as a number in the range [0,1].
In the case of counters, the profile weight for a given profile
point is count for that point divided by the the count for the most
frequently executed profile in the data set.
This provides a single value identifying the relative importance of an
expression and simplifies the combination of multiple profile data sets.
@todo{Still want to say something about percent-of-max vs
percent-of-total and percent-of-average}
@;This percent-of-max value has advantages over percent-of-total and
@;percent-of-average. For example, the ratio of individual counts
@;to the total number of counts is distorted when there are a few heavily
@;executed expressions

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

To demonstrate profile weights, consider the running example from
@figure-ref{if-r-eg}.
Suppose in the first data set, @racket[(flag email 'important)] runs 5
times and @racket[(flag email 'spam)] runs 10 times.
In the second data set, @racket[(flag email 'important)] runs 100 times
and @racket[(flag email 'spam)] run 10 times.
@Figure-ref{profile-weight-comps} shows the profile weights computed
after each data set.
@figure-here["profile-weight-comps" "Sample profile weight computations"
@#reader scribble/comment-reader
@codeblock0|{
;; After loading data from data set 1
(flag email 'important)→ 5/10             ;; 0.5
(flag email 'spam)     → 10/10            ;; 1

;; After loading data from data sets 1 and 2
(flag email 'important)→ (.5 + 100/100)/2 ;; 0.75
(flag email 'spam)     → (1 + 10/100)/2   ;; 0.55
}|]

@section[#:tag "design-source-obj"]{Source objects}
@todo{I don't like `profile points'}
To store and access profile information, we need to associate profile
points with profile information.
We use @emph{source objects}@~cite[dybvig93] to uniquely identify
profile points.
Source objects are typically introduced by the lexer and parser for a
source language and maintained throughout the compiler to correlate
source expressions with intermediate or object code.
This enables both error messages and debuggers to refer to source
expressions instead of target or intermediate representations.

We reuse source objects in our approach to uniquely identify profile
points. As source objects uniquely identify every source expression, the
provide very fine-grain profile information. If two expressions are
associated with the same source object, then they both increment the
same profile counter when executed. Conversely, if two expressions are
associated with different source objects, then they increment different
profile counters when executed.

While source objects are typically introduced by the lexer and parser,
we also require the ability to create new source objects in
meta-programs.
This is useful, for instance, when implementing a DSL. You may want to
profile generated expressions separately from any other expression in
the source language.

In the case of our running example, the lexer and parser introduce
source objects for each expression (and subexpression). That is,
separate source objects are created for @racket[#'(if ...)],
@racket[#'(subject-contains-ci email "PLDI")], @racket[#'subject-contains-ci],
@racket[#'email], @racket[#'"PLDI"], @racket[#'(flag email 'spam)], and
so on. Note that @racket[#'flag] and @racket[#'email] appear multiple
times, and will have a unique source object for each occurrence.

When loading profile information from a previous run, we compute profile
weights and store them in an associative map from source objects to
profile weights.
The meta-programmer can access this information using an API
call, such a @racket[profile-query] in our running example.

@section[#:tag "design-instrumenting"]{Efficient Instrumentation}
Adding a profile point for every single source expression requires care
to instrument correctly and efficiently. This section describes
efficient instrumentation techniques that preserve profile points.

As expressions are duplicated or thrown out during optimization, the
profile points must not be duplicated or lost.
The language implementation should consider profile points as effectful
expressions, such as an assignment to an external variable, that should
never be lost or duplicated.
For example, for every profile point the language could generate an
expression @racket[(profile src)], where @racket[src] is the source
object for that profile point, and never lose or duplicate
@racket[(profile src)] expressions.

@todo{Not sure how much of this is actually relevant to profiling
techniques other than counter based. I can imagine timing the entry to a
block, updating timers based on these profile forms... }

To instrument profiling efficiently, @racket[profile] expressions can be
preserved until generating basic blocks.
While generating basic blocks, all the source objects from the profile
expressions can be attached to the basic block in which they appear.
After associating profile points with basic blocks, we can analyze the
blocks to determine which profile points can be calculated in terms of
other profile points.
Instead of emitting counters for each profile points,
some counters are computed as the sum of a list of other counters.
This technique reduces the number of counter increments to at most one
per block, and fewer in practice, using techniques from Burger and
Dybvig@~cite[burger98].

The above infrastructure is also useful for instrumenting block-level
profiling.
Recall that we can generate new source objects.
When generating basic blocks, we attach a newly generated source
objects for that block.
The source objects need to be generated deterministically to remain
stable across different runs.

@section{Source and Block PGO}
One goal of our approach is to complement rather than interfere with
traditional, e.g., basic block-level PGO.
However, since meta-programs may generate different source code after
optimization, the low-level representation will change after
meta-programs perform optimizations.
Therefore we need to instrument and perform source and basic block-level
optimizations separately.
We describe a workflow for our approach via the running example from
@figure-ref{if-r-eg}.

@;First we compile and instrument a program to collect source-level
@;information.
@;We run this program and collect only source-level information.
@;Next we recompile and optimize the program using the source-level
@;information only, and instrument the program to collect block-level
@;information.
@;From this point on, source-level optimizations should run
@;and the blocks should remain stable.
@;We run this program and collect only the block-level information.
@;Finally, we recompile the program
@;with both source-level and block-level information.
@;Since the source information has not changed, the meta-programs generate
@;the same source code, and thus the compiler generates the same blocks.
@;The blocks are then optimized with the correct profile information.

@todo{Lots of `we'}

To get both source-level and block-level optimizations, we first
instrument the program for source profiling.
After running it on representative inputs, we get the profile weights
such as in @figure-ref{profile-weight-comps}.
Next we recompile using that source profile information, and instrument
profiling for basic blocks.
The generated source code, @figure-ref{if-r-eg}, will remain stable as
long as we continue to optimize using the source profile information.
Since the generated source code remains stable, so do the generated basic
blocks.
Now we profile the basic blocks generated from the optimized source
program.
Finally, we use both the source profile information and the basic block
profile information to do profile-guided optimizations via meta-programming
and traditional low-level optimizations
@figure["workflow-in-code" "This workflow in code"
(racketblock0
(run (instrument-source "spam-filter.rkt")
     input1 input2)
(load-source-profile-info)
(run (instrument-blocks
      (optimize-source "spam-filter.rkt"))
     input1 input2)
(load-source-profile-info)
(load-block-profile-info)
(compile (optimize-source "spam-filter.rkt")))]

@section[#:tag "design-api-sketch"]{Complete API sketch}
@#reader scribble/comment-reader
(racketblock0
;; Takes:   A syntax or source object
;; Returns: A profile weight
(define (profile-query-weight x) ...)

;; Takes: The name of a file contains profile
;;   information
;; Returns: Nothing. Updates an associative map
;;   accessible by profile-query-weight
(define (load-profile-info filename) ...)

;; Takes: A file name
;; Returns: Nothing. Saves information from the
;;   profiling system to filename
(define (save-profile-info filename) ...)

;; Takes: A syntax object and a source object
;; Returns: The given syntax object modified to
;;   be associated with the given source object
(define (annotate-syntax syn src) ...)

;; Takes: A syntax object
;; Returns: A deterministically generated but
;;   fresh source object, possibly based on the
;;   source of the given syntax object
(define (make-source-obj syn) ...))

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

@section[#:tag "impl-chez"]{Chez Scheme implementation}
In Chez Scheme, a source object contains a file name and starting and
ending character positions.
The Chez Scheme reader automatically creates and attaches these to each
syntax object read from a file.
Chez Scheme also provides an API to programmatically manipulate source
objects and attach them to syntax@~cite[csug-ch11].

We generate a new source objects by adding a suffix to the file name of
a base source object.
We generated the suffix from a user provided string and an internally
incremented counter.
By basing generated source object on source objects from the
original source program, errors in generated code are
easier to debug as the generated code contains source file information
from the meta-program that generated the code.

Chez Scheme implements exact counter based profiling using the
instrumentation techniques described in @secref{design-instrumenting},
including the basic block instrumentation techniques to perform
traditional PGOs.

The meta-programming system's runtime maintains an associative map of
source objects to profile weights which is updated by API calls. The API
provide by Chez Scheme nearly identical to the one sketched in
@secref{design-api-sketch}.

@section[#:tag "impl-racket"]{Racket implementation}
Racket does not attach separate source objects to syntax.
Instead, the reader attaches the file name, line number, column number,
position, and span are all attached directly to the syntax object.
Racket provides functions for attaching source information when building
a new syntax object.
We provide wrappers to extract source information into separate source
objects, and to merge source objects into Racket syntax objects.
We then generate source objects in essentially the same way as in Chez
Scheme.

We use one of the pre-existing Racket profiling implementation. The
@racket[errortrace] library provides exact profile counters, like the
Chez Scheme profiler.

We implement a library which provides a similar API to the one sketched
in @secref{design-api-sketch}.
This library maintains the map from source objects to profile
information and computers profile weights.
This library is implemented as standard Racket library that can be
called at by meta-program, and requires no changes to either the Racket
implementation or the @racket[errortrace] library.
