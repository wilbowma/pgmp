#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "design"]{Approach and API}
This section presents the high-level design of our approach and presents
a sketch of the API provided to meta-programmers.
Our approach is not specific to a particular profile technique, but for
simplicity our explinations refer to counter-based profile information.

@;In a typical meta-programming situation, a meta-program takes as input
@;a @emph{source program} in a high-level domain-specific language (DSL)
@;and produces a @emph{target program} in some other language, e.g., C,
@;Haskell, or Scheme.
@;To perform arbitrary meta-program optimizations, we might require profile
@;information for arbitrary points in the source program, arbitrary points
@;in the target program, or both.

@section[#:tag "design-source-obj"]{Profile points}
To store and access profile information, we need to associate profile
information with the source expressions on which meta-programs operate.
@emph{Profile points} uniquely identify expressions that should be profiled by
the profiler.
For instance, if two expressions are associated with the same profile point, then they
both increment the same profile counter when executed.
Conversely, if two expressions are associated with different source
objects, then they increment different profile counters when executed.

For fine-grain profiling, each input expression and subexpression can
have a unique profile point.
In the case of our running example, separate profile points are
associated with @racket[#'(if ...)], @racket[#'(subject-contains email
"PLDI")], @racket[#'subject-contains], @racket[#'email],
@racket[#'"PLDI"], @racket[#'(flag email 'spam)], and so on.
Note that @racket[#'flag] and @racket[#'email] appear multiple
times, but each occurance can have a unique profile point.

In addition to profile points associated with input expressions,
meta-programs can also manufacture new profile-points.
Meta-programmers may want to profile generated expressions separately
from any other expression in the source program.

The meta-programmer can access profile information by passing a profile
points, or object with an associated profile point, to the API
call @racket[profile-query] seen in our running example.

@section[#:tag "design-profile-weights"]{Profile Information}
Absolute profile information such as exact counts are incomparable
across different data sets.
Multiple data sets are important to ensure PGOs can optimize for
multiple classes of inputs expected in production.
Instead, our API provides @emph{profile weights}.
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

@section[#:tag "design-api-sketch"]{Complete API sketch}
Here we sketch the API.

To create profile points,
@(racketblock0
(make-profile-point))
generates profile points deterministically, to ensure
profile information of generated profile points from previous runs
is accessible.

To attach profile points to generated expressions,
@(racketblock0
(annotation-expr expr profile-point))
takes an expression, such as a syntax object, and a profile
point, and associates the expression with the profile point.

To access profile information,
@(racketblock0
(profile-query expr))
takes an expression associated with a profile point, and returns the
profile information associated with that profile point.

To store profile information a run,
@(racketblock0
(store-profile-info filename))
takes a filename and stores profile information from
the profiling system to file.

To load profile information a run,
@(racketblock0
(load-profile-info filename))
takes a filename and loads profile information from the file
so the its accessible by @racket[profile-query].

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
