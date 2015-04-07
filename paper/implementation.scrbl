#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
To validate the design principles from @Secref{design}, we need at
least two implementations.
This section describes implementations in Chez Scheme and Racket.
While both languages belong to the Lisp family, they differ in their
meta-programming and profiling facilities.

@section[#:tag "impl-chez"]{Chez Scheme Implementation}
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
source objects to profile weight, which implements
@racket[(current-profile-information)].
The function @racket[profile-query] simply queries this map.
The function @racket[load-profile] updates this map and the function
@racket[store-profile] stores it to a file.

Despite seperately profiling every source expression, the Chez Scheme
profiler generates at most one counter per block, and fewer in
practice.
Chez Scheme implements precise counter-based profiling, using standard
and efficient block-level profiling
techniques@~citea["ball1994optimally" "burger1998infrastructure"].

@section[#:tag "impl-racket"]{Racket implementation}
In Racket, we implement profile points in essentially the same way as in
Chez Scheme---by using source information attached to each syntax
object.
The Racket reader automatically attaches the filename, line number, etc
to every syntax object it reads from a file.
These source locations are used to report errors at their precise
location.

Racket provides an API for attaching source information when building a
new syntax object.
A separate library exists which provides a more extensive API for
manipulating source information.
We use this library to implement @racket[make-profile-point] and
@racket[annotate-expr] in essentially the same way as in Chez Scheme.
The only differences result from the different representations of source
information.

We implement a library which maintains the associate map from source
locations to profile weight, and provide @racket[profile-query],
@racket[store-profile], and @racket[load-profile] as simple Racket
functions that can be called by meta-programs.
We are able to implement all this as user-level a library due to
Racket's advanced meta-programming facilities and the extensive API
provided by an existing Racket profiler.

Racket includes a pre-existing @racketmodname[errortrace] profiling
library.
The @racketmodname[errortrace] library provides counter-based profiling,
similar to the Chez Scheme profiler.
As @racketmodname[errortrace] is a library and provides programmatic
access to the profiler, we do not need to modify the meta-programming
system of Racket or the @racketmodname[errortrace] library in order to
implement our design.

The @racketmodname[errortrace] profiler only profiles function calls.
To correctly implement the @racket[annotate-expr] function, our Racket
implementation wraps any annotated expression in a dummy function call
when profiling.

@section{Source and Block-level PGO}
One goal of our approach is to avoid interfering with traditional, e.g.,
basic block-level PGO, which Chez Scheme also supports.
However, since meta-programs may generate different source code after
optimization, the low-level representation would have to change when
meta-programs perform optimizations.
To solve this problem, the source code is compiled three times in a
specific order.
Doing so ensure profile information remains consistent at both the
source-level and the block-level.
First, we compile while instrumenting the code to profile source expressions.
After running the instrumented program on representative inputs, we get the profile weights
as in @Figure-ref{profile-weight-comps}.
Second, we recompile, using those profile weights to perform
profile-guided meta-program optimizations, while instrumenting
the code to profile basic blocks.
The generated source code, @Figure-ref{sample-macro}, will remain stable as
long as we continue to optimize using the source profile weights.
Because the generated source code remains stable, so do the generated basic
blocks.
After running the instrumented program, we get the profile weights for
the basic blocks generated from the optimized source program.
Third, we recompile using both the profile weights for the source
expressions and for the basic blocks to do both profile-guided
meta-programming and low-level PGOs.

@section[#:tag "impl-overhead"]{Compile-time and Profiling Overhead}
As with any technique for performing profile-guided optimizations, our
approach introduces compile-time overhead for optimizations and runtime
overhead when profiling.

The compile-time overhead of our API is small.
In our implementations, loading profile information is linear in the
number of profile points, and querying the weight of a particular
profile point is constant-time.
The API does not introduce slowdown at @nonbreaking{runtime, however.}

Profile-guided meta-programs may also slow down (or speed up)
compilation, as they run at compile-time.
The slowdown will depend on the complexity of the meta-program.

Profiling overhead is inherited by the profiler used by the
implementation of our technique.
Previous work measured about 9% runtime overhead introduced by the Chez
Scheme profiler@~citea{burger1998infrastructure}.
According to the @racketmodname[errortrace] documentation, profiling
introduces a factor of 4 to 12 slowdown.
Typically, profiling is disabled for production runs of a program, so
this overhead affects only for profiled runs.
