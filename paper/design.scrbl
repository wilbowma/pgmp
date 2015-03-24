#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "design"]{Approach and API}
This section presents the design of our approach and presents
an example of an API provided to meta-programmers by a meta-programming
system using our approach.
Our approach is not specific to a particular profiling technique, but for
simplicity our explanations refer to counter-based profile information.

@section[#:tag "design-source-obj"]{Profile points}
To store and access profile information, we need to associate profile
information with the source expressions on which meta-programs operate.
@emph{Profile points} uniquely identify expressions that should be
profiled by the underlying profiling system.
For instance, if two expressions are associated with the same profile point, then they
both increment the same profile counter when executed.
Conversely, if two expressions are associated with different profile
points, then they increment different profile counters when executed.
The profiling system uses profile points when a program is
instrumented to collect profile information.
When the program is not instrumented to collect profile information,
profile points do not introduce any runtime overhead.

For fine-grained profiling, each input expression and subexpression can
have a unique profile point.
In the case of our running example, separate profile points are
associated with @racket[#'(if ...)], @racket[#'(subject-contains email
"PLDI")], @racket[#'subject-contains], @racket[#'email],
@racket[#'"PLDI"], @racket[#'(flag email 'spam)], and so on.
Note that @racket[#'flag] and @racket[#'email] appear multiple
times, but each occurrence can have a unique profile point.

In addition to profile points associated with input expressions,
meta-programs can also manufacture new profile points.
Meta-programmers may want to generate expressions that are profiled
separately from any other expression in the source program.

The meta-programmer can access profile information by passing a profile
point, or object with an associated profile point, to an API call, such
as the function @racket[profile-query] in our running example.

@section[#:tag "design-profile-weights"]{Profile Information}
Absolute profile information, such as the exact execution count of an
expression, is incomparable across different data sets.
Multiple data sets are important to ensure PGOs can optimize for
multiple classes of inputs expected in production.
Instead, our approach considers @emph{profile weights}.
The profile weight of a profile point in a given data set is the ratio of
the absolute profile information for that profile point to the maximum of
all other profile points.
The profile weight is represented as a number in the range [0,1].
In the case of counter-based profiling, the profile weight for a given profile
point is execution count for that point divided by the the execution
count for the most frequently executed point in the data set.
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
@Figure-ref{if-r-eg}.
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
Here we sketch an example of an API provided by a meta-programming
system using our approach.
The API assumes the underlying language implementation has some way to
profile expressions that are associated with profile points.
The API is only concerned with providing the meta-program with access
that profile information and the ability to manipulate profile points.

To create profile points,
@(racketblock0
(make-profile-point))
generates profile points. This function must generate profile points
deterministically so the meta-program can access the profile information
of a generated profile point across multiple runs.

To attach profile points to expressions,
@(racketblock0
(annotate-expr expr profile-point))
takes an expression, such as a syntax object, and a profile
point, and associates the expression with the profile point. The
underlying profiling system should then profile that expression
separately from any other expression with a different associated profile
point.

To access profile information,
@(racketblock0
(profile-query expr))
takes an expression associated with a profile point, and returns the
profile information associated with that profile point.

To store profile information from a sample run,
@(racketblock0
(store-profile-info filename))
takes a filename and stores the current profile information from the
underlying profiling system to the file.

To load profile information from a previous run,
@(racketblock0
(load-profile-info filename))
takes a filename and loads the profile information from the file, making
the profile information available via @racket[profile-query].
