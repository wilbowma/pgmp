#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@title[#:tag "design" "Design"]
@(require scribble/manual)
This section presents the essential points of our system. We first
discuss how source points are identified and manufactured. We then
discuss what profile information we use and how we handle multiple data
sets. We delay giving implementation details until
@secref{implementation}.

In a typical meta-programming situation, a meta-program takes as input
a @emph{source program} in a high-level domain-specific language (DSL)
and produces a @emph{target program} in some other language, e.g., C,
Haskell, or Scheme.
To perform arbitrary meta-program optimizations, we might require profile
information for arbitrary points in the source program, arbitrary points
in the target program, or both.
We use @emph{source objects}@~cite[dybvig93] to uniquely identify these
points, and the compiler maintains a database associating source objects
with profile information, whenever profile information from earlier
program runs has been supplied.

@section{Source objects}
Source objects are typically introduced by the lexer and parser for a
source language and maintained throughout the compiler to correlate
source with intermediate or object code, enabling both compile-time
source-error messages and source-level debugging, among other things.
While the source objects created by the lexer and parser encapsulate
a source file descriptor and character range for a specific source
expression, source objects can contain other or different information.
Meta programs can make use of this to manufacture new source objects
representing unique points in the target program, perhaps based on
corresponding points in the source program.

@section{Profile weight}
Instead of storing exact counts in the profile database, we store
@emph{weights} instead.
The weight of a source point in a given dataset is the ratio of the exact
count for the source point and the maximum count for any source point,
represented as a floating-point number in the range [0,1].
This provides a single value identifying the relative importance of an
expression and simplifies the combination of multiple profile data sets.

We considered recording absolute counts, but this compilicates the
combination of multiple datasets, since absolute counts from one run to
the next are not directly comparable.
We also considered recording ratios of individual counts to total or
average counts.
In both cases, the results are distorted when there are a few heavily
executed expressions, potentially leading to difficulty distinguishing
profile weights for two less frequently executed expressions.
We also considered using fixed-precision rather than floating point,
but the floating-point representation makes it easy to determine
the importance of a particular expression overall while still
providing substantial precision when comparing the counts for source
points with similar importance.

To understand how we compute profile weights, consider a program with
two loops, @racket[A] and @racket[B]. If @racket[A] is executed 5 times,
and @racket[B] is executed 10 times, we store
@racket[(profile-query-weight A)] @tt{= 5/10 = 0.5} and
@racket[(profile-query-weight B)] @tt{= 10/10 = 1}. To support multiple data
sets, we simply compute the average of these weights. For instance, if
in a second data set @racket[A] is executed 100 times and @racket[B] is
executed 10 times, then @racket[(profile-query-weight A)] @tt{= ((5/10) +
(100/100))/2 = 0.75} and @racket[(profile-query-weight B)] @tt{= ((10/10) +
(10/100))/2 = 0.55}.
@todo{Diagram} Multiple data sets enable reuse and
help the developer collect representative profile data. This is
important to ensure our PGOs can optimize for multiple classes of inputs
expected in production.
