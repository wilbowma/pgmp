#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "implementation" "Implementation"]
@section[#:tag "impl-source-obj"]{Source objects}
In the previous sections we assumed that source objects can be created
arbitrarily, attached to source points in the surface syntax and be used
as keys. Here we describe the Chez and Racket implementations of source
objects.

In Chez Scheme, a source object usually contains a file name and
starting and ending character positions. The Chez Scheme reader
automatically creates and attaches these to each piece of syntax read
from a file.
Chez Scheme also provides an API to programmatically
manipulate source objects@~cite[csug-ch11].
This is useful when using Chez Scheme as
a target language for a DSL with a different surface syntax. Custom
source objects can be attached to target syntax as well to support
profile-guided meta-programming.

To create custom source objects for fresh profile counters, we can
use arbitrary file names and positions. For
instance, in @secref{eg-datatype} we create custom source objects to
profile list and vector operations. We use the Chez API to attach these
to generated syntax objects. In our Chez implementation, these are
created using the function in @figure-ref{make-source-obj-chez}. This
function takes a string prefix and uses a counter to generate a fake
file names and character positions. The generated file name is partly
based on the input syntax in case they show up in error messages.
@figure["make-source-obj-chez" "Chez implementation for generating source objects"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
@(RACKETBLOCK0
(define (make-fresh-source-obj-factory! prefix)
    (let ([n 0])
      (lambda (syn)
        (let* ([sfd (make-source-file-descriptor
                      (format "~a:~a:~a" (syntax->filename syn) prefix n) #f)]
               [src (make-source-object n n sfd)])
          (set! n (add1 n))
          src)))))]

Racket does not attach separate source objects to syntax.
Instead, the file name, line number, column number, position, and span
are all attached directly to the syntax object. We provide wrappers to
extract these into separate source objects to implement the profile
database, and to attach our source objects to Racket syntax objects.
@Figure-ref{make-source-obj-racket} shows the Racket implementation
of the previous function. Again we generate a fake file names, but simply
copies the other information from the input syntax object.
@figure["make-source-obj-racket" "Racket implementation for generating source objects"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
@(RACKETBLOCK0
(define (make-fresh-source-obj-factory! prefix)
  (let ([n 0])
    (lambda (syn)
      (let ([src (struct-copy srcloc (syntax->srcloc syn)
                   [source (format "~a:~a:~a" (syntax-source syn) prefix n)])])
        (set! n (add1 n))
        src)))))]

@section[#:tag "impl-profile-weights"]{Profile weights}
We represent a profile weight as a set of floating point numbers between
0 and 1. We store @racket[#f] (false) when there is no profile
information, and 0 when the counter was never executed. Our examples all
ignore this distinction and assume @racket[profile-query-weight] always
returns a number. As mentioned in @secref{design-profile-weights}, while
the profiler counters track the exact number of times a source point is
reached, the profile information is converted to weights when being
stored in the database. We considered using Scheme fixnums (integers) for
additional speed, but fixnums quickly loose precision, particularly when
working with multiple data sets.

In Chez, we store profile weights by creating a hash table from source
file names to hash tables. Each second level hash table maps the starting
character position to a profile weight. These tables are not updated in
real time, only when a new data set is manually loaded by an API call in
a program or meta-program.

In Racket, we use one of the pre-existing profiling systems. The
@racket[errortrace] library provides exact profile counters, like the
Chez Scheme profiler. We implement several wrappers to provide an API
similar to the API provided by Chez Scheme. All these wrappers are
implemented simply as Racket functions that can be called at compile
time, requiring no change to either the Racket language implementation
or the @racket[errortrace] library.

The Racket implementation does not maintain a database in the way the
Chez Scheme implementation does. Profile information is stored as an
association list mapping source objects to profile counts. Profile
weights are computed on each call to @racket[profile-query-weight].

@section[#:tag "impl-instrumenting"]{Instrumenting code}
In this section we discuss how we instrument code to collect profiling
information efficiently. As we use a preexisting profiling library for
Racket, we only discuss how we instrument Chez Scheme.

The naïve method for instrumenting code to collect source profile
information would be to add a counter for each source expression.
However this method can easily distort the profile counts. As expressions are
duplicated or thrown out during optimizations, the source information is
also duplicated or lost.

Instead we create a separate profile form that is created after macro
expansion. Each expression @racket[_e] that has a source object
attached is expanded internally to @racket[(begin (profile _src) _e)],
where @racket[_src] is the source object attached to @racket[_e]. The
profile form is considered an effectful expression and should
never be thrown out or duplicated, even if @racket[_e] is. This has the
side-effect of allowing profile information to be used for checking
code-coverage of test suites. While the separate profile form has
benefits, it can interfere with optimizations based on pattern-matching
on the structure of expressions, such as those implemented in a nanopass
framework@~citea{keep2013nanopass}.

We keep profile forms until generating basic blocks. While
generating basic blocks, the source objects from the profile forms are
gathered up and attached to the basic block in which they appear. When a
basic block is entered, every instruction in that block will be
executed. For every instruction in the block, the profile counter
must be incremented. So it is safe to increment the counters for all
the instructions in the block at the top of the block.

In our implementation, we minimize the number of counters
incremented at runtime. After generating basic blocks and attaching the
counters to blocks, we analyze the blocks to determine which
counters can be calculated in terms of other counters. If possible, a
counter is computed as the sum of a list of other counters.
This complicates the internal representation of counters and the
generation of counters, but decreases the overhead of profiling. These
techniques are based on the work of Burger and Dybvig@~cite[burger98].
We generate at most one increment per block, and fewer in practice.

To instrument block-level profiling, we reuse the above infrastructure
by creating fake source objects. Before compiling a file, we reset
global initial block number to 0, and create a fake source file
based on the filename. We give each block a source object using the
fake filename and using the blocks number as the starting and ending
file position.

@;@section{Storing and Loading profile data}
@;We store profile data by creating a hash table from source file names to
@;hash tables. Each second level hash table maps the starting file position
@;of the expression to the weighted count of the expression. This lookup
@;table is only populated after loading profile data from a file and not
@;from a current profiled run.  After loading profile data, it is
@;accessible through @racket[profile-query-weight].
@;
@;Profile data is not immediately loaded into the lookup table after a
@;profiled run of a program. Profile data must first be dumped via
@;@racket[profile-dump-data] and then loaded via
@;@racket[profile-load-data].
@;
@;To dump profile data, the run time gathers up all profile counters.
@;Recall that some counters are computed indirectly in terms of other
@;counters. The values for these indirect counters are computed. These
@;values with their associated source objects are then written to a file.
@;todo{I'm not 100% sure about how this works and I need to be. Some of
@;the racket peoples were asking.}
@;
@;To support loading multiple data sets, we do not load execution counts
@;directly into the lookup table. Instead we compute the percent of max
@;for each counter. Before loading a new data set, we find the maximum
@;counter value.  Each weighted count is computed as a percent of the
@;maximum counter value. If an entry for a source already exists in the
@;lookup table then we compute the weighted average of the previous entry
@;and the counter we're currently loading. We store the weighted count and
@;the current weight in the lookup table, incrementing the weight by one
@;with each new data set.

@section[#:tag "impl-source-block"]{Source and block PGO}
In this section we discuss how we use source and block-level PGO in our
mechanism. Again this section is only relevant to our Chez Scheme
implementation.

When designing our source level profiling system, we wanted to continue
using prior work on low level profile-guided optimizations
@~citea["hwu89" "pettis90" "gupta02"]. However, optimizations based on
source-level profile information may result in a different set of
blocks, so the block-level profile information will be stale. Therefore
optimization using source profile information and those using block
profile information cannot be done after a single profiled run of a
program.

To take advantage of both source and block-level PGO, first we compile
and instrument a program to collect source-level information. We run
this program and collect only source-level information. Next we
recompile and optimize the program using the source-level information
only, and instrument the program to collect block-level information.
From this point on, source-level optimizations should run
and the blocks should remain stable.  We run this program and collect
only the block-level information.  Finally, we recompile the program
with both source-level and block-level information. Since the source
information has not changed, the meta-programs generate the same source
code, and thus the compiler generates the same blocks. The blocks are
then optimized with the correct profile information.
