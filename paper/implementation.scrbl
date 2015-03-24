#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
In this section we describe the instantiations of our approach in Chez
Scheme and in Racket, and discuss compile-time and profiling overhead.

@section[#:tag "impl-chez"]{Chez Scheme Implementation}
Chez Scheme implements profile points using @emph{source
objects}@~cite[dybvig93].
Chez Scheme source objects contain a filename and starting and ending
character positions.
Source objects uniquely identify source expressions, providing
fine-grained profile information.
The Chez Scheme reader automatically creates and attaches them to each
syntax object it reads from a file. The Chez system uses them to
report errors at their precise source location.

Chez Scheme provides an API to programmatically manipulate source
objects and attach them to syntax objects@~cite[csug-ch11].
This API is used to implement @racket[make-profile-point] and
@racket[annotate-expr].
We implement @racket[make-profile-point] to deterministically generating
fresh source objects by adding a suffix to the filename of a base source
object.
This has the added benefit of preserving source locations for error
messages when errors occur in the output of a profile-guided
optimization.
The runtime system maintains an associative map of source
objects to profile weights, which is used to implement
@racket[profile-query].
This associative map is updated via @racket[load-profile-info] and saved
to disk via @racket[store-profile-info].

Chez Scheme implements precise counter-based profiling.
Adding a profile point for every single source expression requires care
to instrument correctly and efficiently.
As optimizations duplicate or throw out expressions, profile points
must not be duplicated or lost.

To ensure profile points are not duplicated, lost, or otherwise
misplaced, Chez Scheme makes profile points explicit in the intermediate
languages by generating an expression @racket[(profile e)], where
@racket[e] is a source object.
Chez Scheme considers these @racket[profile] expressions to be effectful, like
an assignment to an external variable, and never duplicates or removes
them.
Even if the original expression associated with the @racket[profile]
form is duplicated or removed, the @racket[profile] form itself is
preserved separately.
This ensures profile points are not conflated or lost during
compilation.

Chez Scheme preserves @racket[profile] expressions until it generates
basic blocks.
While generating basic blocks, all the profile points from
@racket[profile] expressions are attached to the basic block in which
they appear.
@note{Chez Scheme reuses this infrastructure to profile basic blocks by
generating a new profile point for each basic block.}
Chez Scheme generates at most one counter per block, and fewer in
practice, providing efficient, precise counter-based
profiling@~citea{burger1998infrastructure}.

@;We reuse the source profiling infrastructure to instrumenting
@;block-level profiling.
@;Recall that we can generate new source objects.  When generating basic
@;blocks, we attach a newly generated source objects for that block.
@;The source objects need to be generated deterministically to remain
@;stable across different runs.

@subsection{Source and Block-level PGO}
One goal of our approach is to avoid interfering with
traditional, e.g., basic block-level PGO, which Chez Scheme also
supports.
However, since meta-programs may generate different source code after
optimization, the low-level representation will change when
meta-programs perform optimizations.
We describe a workflow for using both source-level and basic block-level
PGOs via the running example from @Figure-ref{if-r-eg}.

This workflow requires compiling the code three times in a specific
order, to ensure profile information remains consistent at both the
source-level and the block-level.
First, we compile while instrumenting the code to profile source expressions.
After running the instrumented program on representative inputs, we get the profile weights
as in @Figure-ref{profile-weight-comps}.
Second, we recompile, using those profile weights to perform
profile-guided meta-program optimizations, while instrumenting
the code to profile basic blocks.
The generated source code, @Figure-ref{if-r-eg}, will remain stable as
long as we continue to optimize using the source profile weights.
Because the generated source code remains stable, so do the generated basic
blocks.
After running the instrumented program, we get the profile weights for
the basic blocks generated from the optimized source program.
Third, we recompile using both the profile weights for the source
expressions and for the basic blocks to do both profile-guided
meta-programming and low-level PGOs.

@section[#:tag "impl-racket"]{Racket implementation}
In Racket, we implement profile points using source information attached
to each syntax object.
Racket attaches the filename, line number, etc to every syntax object,
and provides functions for attaching source information when building
a new syntax object.
This source information is similar to source objects in Chez Scheme.
We implement @racket[make-profile-point] and @racket[annotate-expr] in
essentially the same way as in Chez Scheme.
The only differences result from the different representations of source
information.

The Racket implementation uses a pre-existing @racketmodname[errortrace]
Racket profiling library.
The @racketmodname[errortrace] library provides counter-based profiling, like
the Chez Scheme profiler.
As @racketmodname[errortrace] is a library and provides programmatic
access to the profiler, we do not need to modify the runtime system of
Racket or the @racketmodname[errortrace] library in order to implement
the rest of the API.
We implement a library which maintains the associate map from source
location to profile weight, and provide @racket[profile-query],
@racket[store-profile-info], and @racket[load-profile-info] as
simple Racket functions that can be called by meta-programs.

@section[#:tag "impl-overhead"]{Compile-time and Profiling Overhead}
As our API runs at compile-time, using it can slow the compilation
process. However, this slowdown is small.
In our implementations, loading profile information is linear in the
number of profile points, and querying the weight of a particular
profile point is constant-time.
The API does not introduce slowdown at runtime, however.

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
this overhead affects only for profiled runs during development.
