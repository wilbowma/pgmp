#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
Here we present parts of our implementations as an example.

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
incrememnted counter.
By basing generated source object on source objects from the
original source program, errors in generated code are
easier to debug as the generated code contains source file information
from the meta-program that generated the code.

Chez Scheme implements exact counter based profiling using the
instrumentation techniques described in @secref{design-instrumenting}.
Chez Scheme also features block-level PGOs such as reordering basic block
based on profile information.

@section[#:tag "impl-racket"]{Racket implementation}
Racket does not attach separate source objects to syntax.
Instead, the file name, line number, column number, position, and span
are all attached directly to the syntax object.
We provide wrappers to extract source information into separate source
objects, and to merge source objects into Racket syntax objects.
We then generate source objects in essentially the same way as in Chez
Scheme.

We use one of the pre-existing Racket profiling implementation. The
@racket[errortrace] library provides exact profile counters, like the
Chez Scheme profiler.
We implement several wrappers to provide an API similar to the API
provided by Chez Scheme.
All these wrappers are implemented simply as Racket functions that can
be called at compile time, requiring no change to either the Racket
implementation or the @racket[errortrace] library.


