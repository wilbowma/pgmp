#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/figure)

@title[#:tag "study-datatype"]{Data Structure Specialization}
In this final case study, we show that our approach is expressive enough
to implement and improve upon state-of-the-art profile-guided
tools such as Perflint@~citea{liu09}, which provides high-level
recommendations for changes in data structures and algorithms that may
result in asymptotic improvements.
We describe implementations of list and vector libraries that warn the
programmer when a different representation may lead to asymptotic
performance gains.
The new libraries wrap the standard list and vector functions.
These wrapper use generated profile point to separately profile each
instance of the data structures.
Finally, we develop a sequence datatype that will automatically
specialize to a list or vector based on profiling information.
As this is done via a library, programmers can easily opt-in to such
automated high-level changes without many changes to their code.
The full implementation of the list library is 80 lines long,
the vector library is 88 lines long, and the sequence library is 111
lines long.
@figure**["profile-list" (elem "Implementation of profiled " @racket[list])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(struct list-rep (instr-op-table ls))
....
(define-syntax (profiled-list syn)
  ;; Create fresh profile points.
  ;; Use list-src to profile operations that are asymptotically fast on lists
  ;; Use vector-src profile operations that are asymptotically fast on vectors
  (define list-src (make-profile-point))
  (define vector-src (make-profile-point))
  ....
  (syntax-case syn ()
    [(_ init-vals ...)
     (unless (>= (profile-query list-src) (profile-query vector-src))
       ;; Prints at compile time.
       (printf "WARNING: You should probably reimplement this list as a vector: ~a\n" syn))
     #`(make-list-rep
         ;; Build a hash table of instrumented calls to list operations
         ;; The table maps the operation name to a profiled call to the
         ;; built-in operation.
         (let ([ht (make-eq-hashtable)])
           (hashtable-set! ht 'car #,(instrument-call car list-src))
           ...
           ht)
         (list init* ...))])))]

@Figure-ref{profile-list} shows the implementation of the profiled list
constructor.
This constructor has the same interface as the standard Scheme list
constructor---it takes an arbitrary number of elements and returns a
representation of a linked list.
The representation of a @racket[profiled-list] is a pair of the
underlying linked list and a hash table of profiled operations.
That is, each instance of a @racket[profiled-list] contains a table
of instrumented calls to the underlying list operations.
The profiled list constructor generates these instrumented operations by
wrapping the underlying list operations with the appropriate profile
point.
The constructor generates two profile points for each profiled list.
One is used to profile operations that are asymptotically fast on lists
and the other is used to profile operations that are asymptotically fast
on vectors.
Finally, the library exports new versions of the list operations that
work on the profiled list representation.
For instance, it exports @racket[car], which takes a
@racket[profiled-list], and uses the instrumented call to @racket[car]
from the hash table of the profiled list on the underlying linked list.
When profiling information already exists, for instance, after a
profiled run, this list constructor emits a warning (at compile time) if
fast vector operations were more common than fast list operations.
We provide an analogous implementation of vectors.
This approach would scale to the other data structures analyzed by
Perflint.

Our approach enables us to go beyond just providing recommendations.
Because our meta-programs are integrated into the language, rather than
separate tools outside the language, we can easily provide libraries
that automatically follow these recommendations rather than asking
programmers to change their code.
To demonstrate this point, we implement a profiled sequence datatype
that will automatically specialize each instance to a list or vector, at
compile-time, based on profile information.

@Figure-ref{profile-seq} shows the implementation of the profiled
sequence constructor.
The code follows the same pattern as the profiled list.
The key difference is we conditionally generate wrapped versions of the
list @emph{or} vector operations, and represent the underlying data
using a list @emph{or} vector, depending on the profile information.
@figure**["profile-seq" "Implementation of profiled sequence"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(struct seq-rep (instr-op-table s))
....
(define-syntax (seq syn)
   (define list-src (make-profile-point))
   (define vector-src (make-profile-point))
   (define previous-list-usage (profile-query list-src))
   (define previous-vector-usage (profile-query vector-src))
   (define list>=vector (>= previous-list-usage previous-vector-usage))
   ....
   (syntax-case syn ()
     [(_ init* ...)
      #`(let ()
          (make-seq-rep
            (let ([ht (make-eq-hashtable)])
                #`(hashtable-set! ht 'car #,(pick-op list>=vector 'car))
                ...
                ht)
            (#,(if list>=vector #'list #'vector) init* ...)))])))]
