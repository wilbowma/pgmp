#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
In this section we instantiation of our approach in Chez Scheme and in
Racket.

@section[#:tag "impl-chez"]{Chez Scheme implementation}
Chez Scheme implements exact counter based profiling.
Adding a profile point for every single source expression requires care
to instrument correctly and efficiently.
As expressions optimizatiosn duplicate or throw out expressions, the
profile points must not be duplicated or lost.

For every profile point Chez Scheme generates an
expression @racket[(profile src)], where @racket[src] is the source
object for that profile point, treats this @racket[profile] expression
as an effectful expression that must not be removed or duplicated.
To instrument profiling efficiently, @racket[profile] expressions are
preserved until generating basic blocks.
While generating basic blocks, all the source objects from the profile
expressions can be attached to the basic block in which they appear.
@note{We reuse this infrastructure to profile basic blocks by generating
a new profile point for each basic block.}
Using techniques from @citet[burger98], we generate at most one counter
per block, and fewer in practice.

@;We reuse the source profiling infrastructure to instrumenting
@;block-level profiling.
@;Recall that we can generate new source objects.  When generating basic
@;blocks, we attach a newly generated source objects for that block.
@;The source objects need to be generated deterministically to remain
@;stable across different runs.

Chez Scheme implements profile points using @emph{source
objects}@~cite[dybvig93] to uniquely identify profile points.
Chez Scheme source objects contains a file name and starting and ending character
positions.
Source objects uniquely identify every source expression,
providing fine-grain profile information.
The Chez Scheme reader automatically creates and attaches these to each
syntax object read from a file, using them, e.g., to give error messages
in terms of source locations.
Chez Scheme also provides an API to programmatically manipulate source
objects and attach them to syntax@~cite[csug-ch11].

We generate a new source objects by adding a suffix to the file name of
a base source object.
By basing generated source objects on source objects from the original
source program, errors in generated code are easier to debug as the
generated code contains source file information from the meta-program
that generated the code.
The meta-programming system's runtime maintains an associative map of
source objects to profile weights which is updated by API calls. The API
provide by Chez Scheme nearly identical to the one sketched in
@secref{design-api-sketch}.

@subsection{Source and Block PGO}
One goal of our approach is to complement rather than interfere with
traditional, e.g., basic block-level PGO, which Chez Scheme also
supports.
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

@section[#:tag "impl-racket"]{Racket implementation}
The Racket implementation uses a pre-existing Racket profiling
implementation call @racket[errortrace].
The @racket[errortrace] library provides exact profile counters, like
the Chez Scheme profiler.

The Racket implemention implements profile points by using source
information attached to each syntax object.
Racket attaches the file name, line number, etc to every syntax object,
and provides functions for attaching source information when building
a new syntax object.
Our implementation provides wrappers to extract source information into
separate source objects, and to merge source objects into Racket syntax
objects.
We then generate source objects in essentially the same way as in Chez
Scheme.

We implement a library which provides a similar API to the one sketched
in @secref{design-api-sketch}.
This library maintains the map from source objects to profile
information and computers profile weights.
This library is implemented as a standard Racket library that can be
called by meta-programs, and requires no changes to either the Racket
implementation or the @racket[errortrace] library.
