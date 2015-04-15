#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "design"]{Design}
Profile-guided meta-programming requires that the underlying language
comes with a profiling system and that the meta-programming system can
associate profile information with source expressions.
This section presents the abstractions introduced by our design and
sketches an API that suffices to support profile-guided
meta-programming.
For simplicity, our explanations refer to counter-based profiling.
Our design should work for other point profiling systems, but does not
extend to path profiling.

@section[#:tag "design-source-obj"]{Profile Points}
As the profiling system may not understand source expressions, our
design introduces @emph{profile points} as an abstraction of source
expressions for the profiler.
Each profile point uniquely identifies a counter.
Any expression can be associated with at most one profile point.
Associating a profile point with an expression indicates which counter
to increment when profiling the expression.
For instance, if two expressions are associated with the same profile
point, then they both increment the same counter when executed.
Conversely, if two expressions are associated with different profile
points, then they increment different profile counters when executed.
The profiling system uses profile points when a program is instrumented
to collect profile information.
When the program is not instrumented to collect profile information,
profile points need not introduce any overhead.

For fine-grained profiling, each node in the AST of a program can be
associated with a unique profile point.
In the case of our running example, the AST nodes for @racket[if],
@racket[subject-contains], @racket[email], @racket["PLDI"], etc, are
each associated with separate profile points.
Note that @racket[flag] and @racket[email] appear multiple times, but
each occurrence is associated with different profile point.

A profiler may implicitly insert profile points on certain nodes in the
AST, but it is also important that meta-programs can manufacture new
profile points.
Meta-programmers may want to generate expressions that are profiled
separately from any other expression in the source program.

Meta-programs can access profile information by passing a profile
point, or an object with an associated profile point, to an API call,
such as the function @racket[profile-query] in our running example.

@section[#:tag "design-profile-weights"]{Profile Weights}
Our design introduces @emph{profile weights} as an abstraction of
the profile information provided by the underlying profiling system.
Profile weights serve two purposes.

First, a profile weight provides a single value identifying the relative
importance a profile point.
The profile weight is represented as a number in the range [0,1].
The profile weight of a profile point is the ratio of the counter for
that profile point to the counter of the most executed profile point in
the same data set.

Second, profile weights simplify merging multiple profile data sets.
Multiple data sets are important to ensure PGOs can optimize for
multiple classes of inputs expected in production.
However, absolute profile information is generally incomparable across different
data sets.
On the other hand, merging the profile weights computed from multiple
data sets is straightforward---the computation is essentially
a weighted average across the data sets.
@figure-here["profile-weight-comps" "Example profile weight computations"
@#reader scribble/comment-reader
@(racketblock0
;; After loading data from data set 1
(flag email 'important)→ (unsyntax (racketvalfont "5/10"))              ;; @racket[0.5]
(flag email 'spam)     → (unsyntax (racketvalfont "10/10"))             ;; @racket[1]

;; After loading data from data sets 1 and 2
(flag email 'important)→ (.5 + (unsyntax (racketvalfont "100/100")))/2 ;; @racket[0.75]
(flag email 'spam)     → (1 + (unsyntax (racketvalfont "10/100")))/2    ;; @racket[0.55]
)]

Consider the running example from @Figure-ref{sample-macro}.
Suppose in the first data set, @racket[(flag email 'important)] runs 5
times and @racket[(flag email 'spam)] runs 10 times, while in the second
data set, @racket[(flag email 'important)] runs 100 times and
@nonbreaking{@racket[(flag email 'spam)]} run 10 times.
@Figure-ref{profile-weight-comps} shows the resulting profile weights
and how to merge the profile weights of these two @nonbreaking{data
sets.}

@section[#:tag "design-api-sketch"]{API}
This section presents an example of an API that implements our design.
We assume an object, @racket[(current-profile-information)],
exists in the meta-programming system.
@Figure-ref{api-sketch} documents the methods of this object.
The API assumes that the underlying profiler has some way to
profile expressions that are associated with profile points.
The API is concerned only with interfacing meta-programs and the
profiler.
The type @racket[SyntaxObject] stands for the type of source expressions
on which meta-programs operate.
@todo{I would rather the documentation not be centered.}
@figure-here["api-sketch" "API Sketch"
@#reader scribble/comment-reader
@(racketblock0
type ProfilePoint
type ProfileWeight
type ProfileInformation)

@defproc[(make-profile-point) ProfilePoint]{
Generates a profile point deterministically so meta-programs can access
the profile information of the generated profile point across multiple
runs.
}

@defproc[(annotate-expr [e SyntaxObject] [pp ProfilePoint])
         SyntaxObject]{
Associates the expression @racket[e] with the profile point @racket[pp].
The profile point @racket[pp] replaces any other profile point with
which @racket[e] is associated.
The underlying profiling system increments the counter for
@racket[pp] any time @racket[e] is executed.
}

@defproc[(profile-query [e SyntaxObject])
         ProfileWeight]{
Retrieves the profile weight associated with the profile point for the
expression @racket[e].
}

@defproc[(store-profile [f Filename])
         Null]{
Stores the current profile information from the underlying profile
system in the file with the filename @racket[f].
}

@defproc[(load-profile [f Filename])
         ProfileInformation]{
Loads the profile information stored in the file with the filename
@racket[f].
}
]
