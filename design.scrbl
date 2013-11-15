#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@title[#:tag "design" "Design"]
@(require scribble/manual)
This section presents the essential points of our system. We first
discuss how source points are identified and manufactured. We then
discuss what profile information we use and how we handle multiple data
sets. We elide implementation details until
@secref{implementation}.

@section{Source objcets}
To perform arbitrary meta-program optimizations, we require profile
information from arbitrary points in the source program. We model this
using source objects, which act as unique keys to how often a particular
point in the code is reached. Each source expression of a
program is annotated with a unique source object. We can create new
(fresh) source objects using @racket[(make-source-object)], and can
create a new profile point by using @racket[(profile _src)], where
@racket[_src] is some source object. We access the profile information
through the function @racket[profile-query-weight], which takes a source
object or source expression and returns a number representing the
execution frequency.

@section{Profile weight}
Instead of exact counts, we store execution counts relative to the most
frequently executed expression in the program. This provides a single
value that represents the relative importance of an expression and
supports using multiple profile data sets. These profile weights are
associated with each source object, and returned by
@racket[profile-query-weight]. 

We considered comparing to the total number of expressions executed or
the average number of times an expression is executed.  In both cases,
the results are distorted when there are a large number of expressions
that are executed infrequently. In that case, a main loop might look
infrequently executed if there are many start up or shut down steps. By
comparing to the most expensive expression, we have a relatively stable
comparison of how expensive some expression is, even in cases with many
unused expressions or a few very expensive expressions.

To understand how we compute profile weights, consider a program with
two loops, @racket[A] and @racket[B]. If @racket[A] is executed 5 times,
and @racket[B] is executed 10 times, we store
@racket[(profile-query-weight A)] @tt{= 5/10 = 0.5} and
@racket[(profile-query-weight B)] @tt{= 10/10 = 1}. To support multiple data
sets, we simple compute the average of these weights. For instance, if
in a second data set @racket[A] is executed 100 times and @racket[B] is
executed 10 times, then @racket[(profile-query-weight A)] @tt{= ((5/10) +
(100/100))/2 = 0.75} and @racket[(profile-query-weight B)] @tt{= ((10/10) +
(10/100))/2 = 0.55}.
@todo{Diagram} Multiple data sets enable reuse and
help the developer collect representative profile data. This is
important to ensure our PGOs optimize for the class of inputs we expect
in production.
