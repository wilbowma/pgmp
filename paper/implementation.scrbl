#lang scribble/base
@(require
  "defs.rkt"
  "bib.rkt"
  scribble/manual
  scriblib/footnote
  scriblib/figure)

@title[#:tag "implementation" "Implementations"]
In this section we describe the instantiations of our approach in Chez
Scheme and in Racket, discuss compile-time and profiling overhead, and
briefly describe other meta-programming systems in which our approach
should apply.

@section[#:tag "impl-chez"]{Chez Scheme implementation}
Chez Scheme implements profile points using @emph{source
objects}@~cite[dybvig93].
Chez Scheme source objects contains a filename and starting and ending
character positions.
Source objects uniquely identify every source expression, providing
fine-grain profile information.
The Chez Scheme reader automatically creates and attaches these to each
syntax object read from a file, using them to give error messages in
terms of source locations, among other things.
Since the reader automatically attaches source objects to each
expression in a file, each source expression comes with a unique,
deterministically generated profile point.
Chez Scheme also provides an API to programmatically manipulate source
objects and attach them to syntax objects@~cite[csug-ch11].
This API is used to implement @racket[make-profie-point] and
@racket[annotat-expr].

We deterministically generate a fresh source objects by adding a suffix to
the filename of a base source object.
By basing generated source objects on those from the original source
program, errors in generated code are easier to debug as the generated
code contain source information from the file in which the code was
generated.
The meta-programming system maintains an associative map of source
objects to profile weights, which is updated by API calls.
The API provided by Chez Scheme is nearly identical to the one sketched
in @secref{design-api-sketch}.

Chez Scheme implements counter-based profiling.
Adding a profile point for every single source expression requires care
to instrument correctly and efficiently.
As optimizations duplicate or throw out expressions, the profile points
must not be duplicated or lost.

To ensure profile points are not duplicated, lost, or otherwise mixed
up, Chez Scheme makes profile points explicit in the intermediate
langauges by generating an expression @racket[(profile e)], where
@racket[e] is a source object.
Chez Scheme treats these @racket[profile] expressions as effectful, like
an assignment to an external variable, and never duplicates or removes
them.
Even if the original expression associated with the @racket[profile]
form is duplicated or removed, the @racket[profile] form itself is
preserved separately.
This ensures profile points are not conflated or lost during
compilation.

To instrument profiling efficiently, @racket[profile] expressions are
preserved until generating basic blocks.
While generating basic blocks, all the profile points from
@racket[profile] expressions are attached to the basic block in which
they appear.
@note{Chez Scheme reuses this infrastructure to profile basic blocks by
generating a new profile point for each basic block.}
Using techniques from @citeta{burger1998infrastructure}, Chez Scheme generate at most one
counter per block, and fewer in practice.

@;We reuse the source profiling infrastructure to instrumenting
@;block-level profiling.
@;Recall that we can generate new source objects.  When generating basic
@;blocks, we attach a newly generated source objects for that block.
@;The source objects need to be generated deterministically to remain
@;stable across different runs.

@subsection{Source and Block PGO}
One goal of our approach is to complement rather than interfere with
traditional, e.g., basic block-level PGO, which Chez Scheme also
supports.
However, since meta-programs may generate different source code after
optimization, the low-level representation will change after
meta-programs perform optimizations.
Therefore, we need to instrument and perform source-level and basic
block-level optimizations separately.
We describe a workflow for using both source-level and basic block-level
PGOs via the running example from @figure-ref{if-r-eg}.

To get both source-level and block-level optimizations, we first
instrument profiling for source expressions.
After running it on representative inputs, we get the profile weights
such as in @figure-ref{profile-weight-comps}.
Next, we recompile using that source profile information, and instrument
profiling for basic blocks.
The generated source code, @figure-ref{if-r-eg}, will remain stable as
long as we continue to optimize using the source profile information.
Since the generated source code remains stable, so do the generated basic
blocks.
Now we profile the basic blocks generated from the optimized source
program.
Finally, we use both the source profile information and the basic block
profile information to do both profile-guided meta-programming
and low-level PGOs.

@section[#:tag "impl-racket"]{Racket implementation}
In Racket, we implement profile points using source information attached
to each syntax object.
Racket attaches the filename, line number, etc to every syntax object,
and provides functions for attaching source information when building
a new syntax object.
Our implementation provides wrappers to extract source information into
separate source objects, and to merge source objects into Racket syntax
objects.
We then generate source objects in essentially the same way as in Chez
Scheme.

The Racket implementation uses a pre-existing Racket profiling
library called @racket[errortrace].
The @racket[errortrace] library provides counter-based profiling, like
the Chez Scheme profiler.

We implement a library which provides a similar API to the one sketched
in @secref{design-api-sketch}.
This library maintains the map from source objects to profile
information and computes profile weights.
This library is implemented as a standard Racket library that can be
called by meta-programs, and requires no changes to either the Racket
implementation or the @racket[errortrace] library.

@section[#:tag "impl-overhead"]{Compile-time and Profiling Overhead}
In this section, we briefly discuss the overhead caused by implementing our
API and by implementing optimizations using our approach.

In our implementations, the overhead for loading profile information is
linear in the number of profile points, and the overhead for determining
the weight of a particular profile point is constant-time. In both
cases, the cost per profile point is small---a hash-table write or read.
The overhead for performing an optimization based on profile information
will be specific to the optimization. For instance, conditional branch
reordering---such as @racket[exclusive-cond] in the next section---is a
simple local change and will likely not have much compile-time overhead.
Performing function inlining as a profile-guided meta-program might be
more costly; the meta-program must account for global changes to code
size, may be run multiple times depending on the number of function
calls, and may slow down (or speed up) later stages of compilation.

Profiling overhead is inherited by the profiler used by the
implementation of our technique. Previous work measured about 9%
overhead of the profiler used in our Chez Scheme
implementation@~citea{burger1998infrastructure}. The
profiling library used in our Racket implementation has overhead ranging
from 3% to 33%@~cite[stamour14]. Typically, profiling is disabled for
production runs of a program, so this overhead affects only profiled
runs during development.

@section[#:tag "impl-other"]{Instantiations in other meta-programming systems}
Both of our instantiations are in similar Scheme-style meta-programming
systems, but the approach can work in any sufficiently expressive
meta-programming system.
Languages such as Template Haskell@~citea{sheard2002template}, MetaML@~citea{taha00},
and Scala@~cite[scala-overview-tech-report]
feature powerful meta-programming facilities similar to
that of Scheme@~cite[dybvig93].
They allow executing expressive programs at compile-time, provide direct
access to input expressions, and provide template-style meta-programming
facilities.
C++ template meta-programming is more restricted than the above systems,
so it is not clear how to instantiate our approach for C++ templates.
In this section we briefly sketch
implementation strategies for other general-purpose meta-programming systems.

@subsection{TemplateHaskell}
TemplateHaskell@~citea{sheard2002template}, which is provided with the
current version of the Glasgow Haskell Compilers (GHC) (version 7.8.4),
provides general-purpose meta-programming to Haskell.

GHC has a profiler that profiles "cost-centres". By default,
each function defines a cost-centres, but the user can define new
cost-centres by adding an @emph{annotation} to the source code of the form
@tt{{#- SCC "cost-centre-name" #-}}.
Cost-centres map easily to profile points.

TemplateHaskell, as GHC 7.7, supports generating and querying annotations. Since
cost-centres are defined via annotations, implementing
@racket[make-profile-point], @racket[annotate-expr], and
@racket[profile-query] should be simple.

As the output of the profiler is an easily parsed file that associates time and
allocation with each cost-centre, implementing @racket[load-profile]
should be as simple as writing a parser for the .prof file.
As profiling in Haskell exists largely outside the language and
inside the GHC toolchain, implementing @racket[store-profile] in the
language would not be useful. Instead, this is done using a system
call such as @exec{ghc -prof filename.hs}.

@subsection{MetaOCaml}
MetaOCaml is a reimplementation of MetaML@~citea{taha00} based on OCaml.
MetaOCaml provides general-purpose meta-programming based on multi-stage
programming. The most recent version, BER MetaOCaml N102, is implemented
on top of OCaml version 4.02.1@~citea{bermetaocaml}.

OCaml features a counter-based profiler that associates counts with the
locations of certain source AST nodes. It is unclear how much
location and AST information is currently exposed to MetaOCaml, but
exposing some of these internals would make it simple to implement
@racket[load-profile] and @racket[profile-query]. As the profiler is
based on source locations, implementing @racket[make-profile-point] and
@racket[annotate-expr] may require similar tricks to those used in our
Racket and Chez Scheme implementations.

As with GHC, implementing @racket[store-profile] inside the language is
not useful, and profiling in OCaml is done via a call such as
@exec{ocamlcp -p a filename.ml -o filename; ./filename}.

@subsection{Scala}
Scala features powerful general-purpose meta-programming, including
template-style meta-programming, and various reflection
libraries@~citea{burmako2013scala}.

The only profilers for Scala seem to be at the level of the JVM,
however, JVM bytecode retains much source information. It should be
possible to map the profiling information at the JVM level back to Scala
source code. With such a mapping, a Scala implementation of our API
should be similar to our Racket implementation.
