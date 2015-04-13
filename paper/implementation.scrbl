#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
To validate the design principles from @Secref{design}, we provide two
implementations.
This section describes implementations in Chez Scheme and Racket and
discusses some implementation concerns.
While both languages belong to the Lisp family, they differ in their
meta-programming and profiling facilities.

@section[#:tag "impl-chez"]{Chez Scheme Implementation}
Chez Scheme implements precise counter-based profiling, using standard
and efficient block-level profiling
techniques@~citea["ball1994optimally" "burger1998infrastructure"].
The Chez Scheme profiler effectively profiles every source expression
and provides profiles in terms of source-code locations.

In Chez Scheme, we implement profile points using @emph{source
objects}@~citea{dybvig93} which can be attached to syntax objects.
Chez Scheme source objects contain a filename and starting and ending
character positions.
The Chez Scheme reader automatically creates and attaches source objects
to each syntax object it reads from a file.
Chez Scheme uses source objects to report errors at their precise source
location.

Chez Scheme provides an API to programmatically manipulate source
objects and attach them to syntax objects@~cite[csug-ch11].
We use this API to implement @racket[make-profile-point] and
@racket[annotate-expr].
The former deterministically generates fresh source objects by adding a
suffix to the filename of a base source object.
This scheme has the added benefit of preserving source locations for
error messages when errors occur in the output of a profile-guided
optimization.

We modify the meta-programming system to maintain an associative map of
source objects to profile weights, which implements
@racket[(current-profile-information)].
The function @racket[profile-query] simply queries this map.
The function @racket[load-profile] updates this map from a file and the
function @racket[store-profile] stores it to a file.

@section[#:tag "impl-racket"]{Racket Implementation}
Racket includes an @racketmodname[errortrace] profiling library.
The @racketmodname[errortrace] library provides counter-based profiling
and returns profiles in terms of source code locations, similar to the
Chez Scheme profiler.
Note that in contrast to the Chez Scheme profiler, the
@racketmodname[errortrace] library profiles only function calls.

In Racket, we implement profile points in essentially the same way as in
Chez Scheme---by using source information attached to each syntax
object.
The Racket reader automatically attaches the filename, line number, etc
to every syntax object it reads from a file.
These source locations are used to report errors at their 
@nonbreaking{precise location.}

Racket provides an API for attaching source information when building a
new syntax object.
A separate library exists which provides a more extensive API for
manipulating source information.
We use this library to implement @racket[make-profile-point] and
@racket[annotate-expr] in essentially the same way as in Chez Scheme.
There is one key difference because the @racketmodname[errortrace]
library profiles only functions calls.
When annotating an expression @racket[e] with profile point @racket[p],
we generate a new function @racket[f] whose body is @racket[e].
The result of @racket[annotate-expr] is a call to the generated function
@racket[f].
This call to @racket[f] is annotated with the profile point @racket[p].
While this results in different performance characteristics while
profiling, it does not change the counters used to calculate profile
weights.

We implement a library that maintains the associative map from source
locations to profile weights.
The library provides our API as simple Racket functions that can be
called by meta-programs.
We are able to implement the entire API as a user-level library due to
Racket's advanced meta-programming facilities and the extensive API
provided by the @racketmodname[errortrace] profiler.

@section{Source and Block-level PGO}
One goal of our approach is to avoid interfering with traditional, e.g.,
basic-block-level PGO, which Chez Scheme also supports.
However, since meta-programs may generate different source code after
optimization, the low-level representation would have to change when
meta-programs perform optimizations.
The different low-level code would invalidate the low-level profile
information.
To solve this problem, the source code is compiled three times in a
specific order, instead of the usual two times.
Doing so ensures profile information remains consistent at both the
source-level and the block-level.
First, we compile while instrumenting the code to profile source expressions.
After running the instrumented program on representative inputs, we get
the profile weights as in @Figure-ref{profile-weight-comps}.
Second, we recompile, using those profile weights to perform
profile-guided meta-program optimizations, while instrumenting
the code to profile basic blocks.
The generated source code, @Figure-ref{sample-macro}, will remain stable
as long as we continue to optimize using the source profile weights.
Because the generated source code remains stable, so do the generated
basic blocks.
After running the instrumented program, we get the profile weights for
the basic blocks generated from the optimized source program.
Third, we recompile using both the profile weights for the source
expressions and for the basic blocks to do both profile-guided
meta-programming and low-level PGOs.

@section[#:tag "impl-overhead"]{Compile-Time and Profiling Overhead}
As with any technique for performing profile-guided optimizations, our
approach introduces compile-time overhead for optimizations and run-time
overhead when profiling.

The compile-time overhead of our API is small.
In our implementations, loading profile information is linear in the
number of profile points, and querying the weight of a particular
profile point is amortized constant-time.
Since they run at compile time, a profile-guided meta-program might
slow down or speed up compilation, depending on the complexity
of the meta-program and whether it produces more or less code as
a result of the optimization.

The API does not directly introduce run-time overhead; however,
a meta-programming system using our technique inherits overhead from the
profiler used in the implementation.
Previous work measured about 9% run-time overhead introduced by the Chez
Scheme profiler@~citea{burger1998infrastructure}.
According to the @racketmodname[errortrace] documentation, the profiler
introduces a factor of 4 to 12 slowdown.
This does not include the additional instrumentation our implementation
of @racket[annotate-expr] performs, i.e., wrapping each annotated
expression in a function call.
Typically, profiling is disabled for production runs of a program, so
this overhead affects only profiled runs.
