#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure
   racket/port)

@title[#:tag "case-studies"]{Case Studies}
In this section we evaluate the generality of our approach by
implementing existing PGOs and profile-guided meta-programs in the
Racket instantiation of our approach.
We first demonstrate optimizing Scheme's @racket[case], a
multi-way branching construct similar to C's @code{switch}, as a
meta-program.
Then we then implement profile-guided receiver class
prediction@~citea["holzle1994optimizing" "grove95"] for an
object system implemented as a syntax extension.
Finally we implement a sequence datatype that specializes each
instance to a @racket[list] or @racket[vector], based on
profiling information, automating the recommendations performed by tools
like Perflint@~citea{liu09}.

@section[#:tag "study-case"]{Profile-guided conditional branch optimization}
As our first case study, we perform profile-guided optimization of
Scheme's @racket[case] construct, which is similar to C's @code{switch}
statement.
In C#, @code{switch} statements must be mutually exclusive and do not
allow fall through---each case must end in a jump such as @code{break}.
The .NET compiler features a profile-guided optimization of @code{switch}
statements that uses profile information to reorder the branches
according to which branch is most likely to succeed.
This case study shows that our approach is general enough to implement
this optimization. The entire implementation is 65-line.

The @racket[case] construct takes an expression @racket[key-expr] and an
arbitrary number of clauses, followed by an optional @racket[else]
clause.
The left-hand side of each clause is a list of constants.
@racket[case] executes the right-hand side of the first clause in which
@racket[key-expr] is @racket[equal?] to some element of the left-hand
side.
For simplicity, we present a version that assumes the left-hand sides
are mutually exclusive and ignore the @racket[else] clause@note{The full
implementation handles the full generality of Scheme's @racket[case]}.
@Figure-ref{case-example} shows an example @racket[case] expression.
@figure-here["case-example" (elem "An example using " @racket[case])
@(racketblock0
(define (parse stream)
 (case (peek-char stream)
  [(#\space #\tab) (white-space stream)]
  [(0 1 2 3 4 5 6 7 8 9) (digit stream)]
  [(#\() (start-paren stream)]
  [(#\)) (end-paren stream)]
  ...)))]

@; How are clauses parsed
@Figure-ref{case-impl} shows the profile-guided implementation of
@racket[case] that reorders branches according to which clause is most
likely to succeed.
First it parses each clause into a struct containing the list of keys from
the left-hand side, and the expressions from the right-hand side.
Then it generates an invocation of another meta-program,
@racket[exclusive-cond], which reorders its branches based on profile
information.
Each clause of the generated @racket[exclusive-cond] tests if the
key expression is @racket[equal?] to any key in the list of keys from
the @racket[case] clause.
The full implementation of @racket[case] is 41-line.
@figure**["case-impl" (elem "Implementation of " @racket[case])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (case syn)
 (struct case-clause (keys body))
 (define (parse-clause clause)
  (syntax-case clause ()
   [((k ...) e1 e2 ...)
    (make-case-clause #'(k ...) #'(e1 e2 ...))]))
 (syntax-case syn ()
  [(_ key-expr clause ...)
    ;; Evaluate the key-expr only once, instead of
    ;; copying the entire expression in the template.
   #`(let ([t key-expr])
       (exclusive-cond
        ;; Parse clauses and splice a list of generated tests
        ;; into exclusive-cond
        #,@(for/list ([clause (map parse-clause #'(clause ...))])
           ;; Take this branch if the key expression is equal? to some
           ;; key in the list of keys for this branch.
           #`((key-in? t '#,(case-clause-keys clause))
              #,@(case-clause-body clause)))))])))]

@Figure-ref{exclusive-cond}, introduces the @racket[exclusive-cond]
construct, a multi-way conditional branch, like Lisp's
@racket[cond]@note{The full implementation handles other @racket[cond]
syntaxes and @racket[else] clauses.}, that expects all branches to be
mutually exclusive.
Because the branches are mutually exclusive, @racket[exclusive-cond] can
safely reorder them.
@racket[exclusive-cond] simplifies the implementation of @racket[case],
and demonstrates an important feature of profile-guided
meta-programming---meta-programming allows the programmer to encode
their knowledge, e.g., that the branches of this conditional are
mutually exclusive, in their program and take advantage of optimizations
that would have otherwise been impossible.
@figure**["exclusive-cond" (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader
(RACKETBLOCK0
(define-syntax (exclusive-cond syn)
  (struct clause (syn weight))
  (define (parse-clause clause)
    (syntax-case clause ()
      [(test e1 e2 ...) (make-clause clause (profile-query #'e1))]))
  (define (sort-clauses clause*)
    (sort (map parse-clause clause*) > #:key clause-weight))
  (syntax-case x (else)
    [(_ clause ...)
     #`(cond #,@(map clause-syn (sort-clauses #'(clause ...))))])))]

@; How does exclusive-cond use profile information to implement cond
The implementation of @racket[exclusive-cond] parses each clause
into a struct that contains the original clause and the profile
information about how many times that branch was taken.
Then it sorts the clauses by profile weight and generates a regular
@racket[cond].
The full implementation of @racket[exclusive-cond] is 24-line.

@Figure-ref{case-expansion} shows the code generated from the example @racket[case]
expression from @figure-ref{case-example}
@figure-here["case-expansion"
        (elem "Generated code from " @figure-ref{case-example})
@#reader scribble/comment-reader
@(racketblock0
;; After case expands
(define (parse stream)
 (let ([t (peek-char stream)])
   (exclusive-cond
     [(key-in? t '(#\space #\tab))
      (white-space stream)]
     [(key-in? t '(0 1 2 3 4 5 6 7 8 9)) (digit stream)]
     [(key-in? t '(#\()) (start-paren stream)]
     [(key-in? t '(#\))) (end-paren stream)]
     )))
;; After exclusive-cond expands
(define (parse stream)
 (let ([t (peek-char stream)])
   (cond
     [(key-in? t '(#\space #\tab))
      (white-space stream)] ;; Run 55 times
     [(key-in? t '(#\())
      (start-paren stream)] ;; Run 23 times
     [(key-in? t '(#\)))
      (end-paren stream)]   ;; Run 23 times
     [(key-in? t '(0 1 2 3 4 5 6 7 8 9))
      (digit stream)]       ;; Run 10 times
     ))))]

This case study has demonstrated that our approach is general enough to
implement a well-known PGO via meta-programming.

@section[#:tag "study-virtual-call"]{Profile-guided receiver class prediction}
In this case study we provide a meta-program that implements
profile-guided receiver class prediction@~citea["holzle1994optimizing"
"grove95"] for a simplified object system implemented as a syntax
extension.
This demonstrates that our mechanism is both general enough to implement
well-known PGOs, and powerful enough to provide domain-specific
languages with PGOs not available in the host language.
The full implementation of profile-guided receiver class prediction is
44-line, while the implementation of the entire object system (including
receiver class prediction) is 129-line.

@Figure-ref{method-call-impl} shows the implementation of profile-guided
receiver class prediction.
A method call such as @racket[(method shape area)] is actually a
meta-program that generates code as follows.
First, it generates a new profile point for each class in the system.
Then it attaches each profile point to a call to the dynamic dispatch
routine.
When profile data is not available, the implementation generates a @racket[cond]
expression which tests the class of the object and calls the dynamic
dispatch routine, but @emph{with a different profile point for each
branch}.@note{A production implementation would create a table of
instrumented dynamic dispatch calls and dynamically dispatch through
this table, instead of instrumenting code with @racket[cond]. However,
using @racket[cond] simplifies visualizing the instrumentation.}
That is, each method call site is instrumented by generating a multi-way
branch to the standard dynamic dispatch routine, but with a separate
profile point in each branch.
When profile information is available, the method call generates a
@racket[cond] expression that tests for the most frequently used classes
at this method call site, and inlines those methods---that is, it
performs polymorphic inline caching using the profile information.
The full implementation of profile-guided receiver class prediction
is 44-line. The rest of the object system implementation is an
additional 87-line.
@todo{Maybe implement the instrumented hash table later}
@figure**["method-call-impl" "Implementation of profile-guided receiver class prediction"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (method syn)
  (syntax-case syn ()
    [(_ obj m val* ...)
     ...
     ;; Don't copy the object expression throughout the template.
     #`(let* ([x obj])
         (cond
           #,@(if no-profile-data?
                  ;; If no profile data, instrument!
                  (for/list ([d instr-dispatch-calls] [class all-classes])
                    #`((instance-of? x #,class) (#,d x m val* ...)))
                  ;; If profile data, inline up the top inline-limit classes
                  ;; with non-zero weights
                  (for/list ([class (take sorted-classes inline-limit)])
                    #`((instance-of? x #,class)
                       #,(inline-method class #'x #'m #'(val* ...)))))
           ;; Fall back to dynamic dispatch
           [else (dynamic-dispatch x m val* ...)]))])))]

@Figure-ref{method-call-example} shows an example method call, the
resulting code after instrumentation, and the resulting code after
optimization.
Note that each occurrence of @racket[(instrumented-dispatch x area)]
has a different profile point, so each occurrence is profiled separately.
@figure["method-call-example" "Example of profile-guided receiver class prediction"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(class Square
  ((length 0))
  (define-method (area this)
    (sqr (field this length))))
(class Circle
  ((radius 0))
  (define-method (area this)
    (* pi (sqr (field this radius)))))
(class Triangle
  ((base 0) (height 0))
  (define-method (area this)
    (* 1/2 base height)))
...
(for/list ([s (list cir1 cir2 cir3 sqr1)])
  (method s area))

;; ---------------------------
;; After instrumentation
...
(let* ([x c])
  (cond
    [(instance-of? x 'Square)    ;; Run 1 time
     (instrumented-dispatch x area)]
    [(instance-of? x 'Circle)    ;; Run 3 times
     (instrumented-dispatch x area)]
    [(instance-of? x 'Triangle)  ;; Run 0 times
     (instrumented-dispatch x area)]
    [else (dynamic-dispatch x area)]))

;; ---------------------------
;; After optimization
...
(let* ([x c])
  (cond
    [(instance-of? x 'Square)  ;; Run 1 time
     (let ([this x]) (sqr (field x length)))]
    [(instance-of? x 'Circle)  ;; Run 3 times
     (let ([this x]) (* pi (sqr (field x radius))))]
    [else (dynamic-dispatch x area)])))]

As a further improvement, we could reuse @racket[exclusive-cond] to test
for classes in the the most likely order.
@figure["method-call-exclusive-cond" "Profile-guided receiver class prediction, sorted."
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
;; ---------------------------
;; After optimization
...
(let* ([x c])
  (exclusive-cond
    [(instance-of? x 'Square)  ;; Run 1 time
     (let ([this x]) (sqr (field x length)))]
    [(instance-of? x 'Circle)  ;; Run 3 times
     (let ([this x]) (* pi (sqr (field x radius))))]
    [else (dynamic-dispatch x area)]))

;; ---------------------------
;; After more optimization
...
(let* ([x c])
  (cond
    [(instance-of? x 'Circle)  ;; Run 3 times
     (let ([this x]) (* pi (sqr (field x radius))))]
    [(instance-of? x 'Square)  ;; Run 1 time
     (let ([this x]) (sqr (field x length)))]
    [else (dynamic-dispatch x area)])))]


This case study has demonstrated that our approach is general enough to
implement a well-known PGO as a meta-program, and provide
domain-specific languages with PGOs not available in host language.

@section[#:tag "study-datatype"]{Data Structure Specialization}
@; Motivate an example that normal compilers just can't do
In this case study we show that our approach is general enough to implement
tools such as Perflint@~citea{liu09}, which provides high-level
recommendations for changes in data structures and algorithms that may
result in asymptotic improvements.
We then go beyond recommendation and provide the programmer with a
library that automatically specializes each instance of a data structure
according to profile information.
The full implementation of the list library is 80-line, the vector library is
88-line, and the sequence library is 111-line.

We provide implementations of lists and vectors that warn the programmer
when a different representation may lead to asymptotic performance
gains.
The implementations provide wrappers around the standard list and vector
functions, using newly generated profile point to separately profile
each instance of the data structures.
Finally, we provide an implementation of a sequence datatype that will
automatically specialize to a list or vector based on profiling
information.
As this is done via a library, the programmer can easily opt-in to such
automated high-level changes without changing their code.
@figure**["profile-list" (elem "Implementation of profiled " @racket[list])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(struct list-rep (instr-op-table ls))
...
(define-syntax (profiled-list syn)
  ;; Create fresh profile points.
  ;; Use list-src to profile operations that are asymptotically fast on lists
  ;; Use vector-src profile operations that are asymptotically fast on vectors
  (define list-src (make-profile-point))
  (define vector-src (make-profile-point))
  ...
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
This constructor has the same interface as the standard
Scheme list constructor---it takes an arbitrary number of elements and
returns a representation of a linked list.
The representation of a @racket[profiled-list] is a pair of the
underlying linked list and a hash table of profiled operations.
That is, each instance of a @racket[profiled-list] contains a table
of profiled calls to the underlying list operations.
The profiled list constructor generates these profiled operations by
wrapping the underlying list operations with the appropriate profile
point.
It generates two profile points for each profiled list.
One is used to profile operations that are asymptotically fast on lists
and the other is used to profile operations that are asymptotically fast
on vectors.
Finally, the library exports new versions of the list operations
that work on the profiled list representation.
For instance, it exports @racket[car], which takes a @racket[profiled-list],
and uses the profiled call to @racket[car] from the hash table of the
profiled list on the underlying linked list.
When profiling information already exists, for instance, after a
profiled run, this list constructor emits a warning (at compile time) if
fast vector operations were more common than fast list operations.
We provide an analogous implementation of vectors. This approach
would scale to the other data structures analyzed by Perflint.

Our approach enables us to go beyond just providing recommendations.
Because our meta-programs are integrated into the language, rather than
existing as a separate tool outside the language, we can provide
libraries to the programmer that automatically follows these
recommendations rather than asking the programmer to change their code.
To demonstrate this, we implement a profiled sequence datatype that will
automatically specialize each instance to a list or vector, at compile
time, based on profile information.

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
...
(define-syntax (seq syn)
   (define list-src (make-profile-point))
   (define vector-src (make-profile-point))
   (define previous-list-usage (profile-query list-src))
   (define previous-vector-usage (profile-query vector-src))
   (define list>=vector (>= previous-list-usage previous-vector-usage))
   ...
   (syntax-case syn ()
     [(_ init* ...)
      #`(let ()
          (make-seq-rep
            (let ([ht (make-eq-hashtable)])
                #`(hashtable-set! ht 'car #,(pick-op list>=vector 'car))
                ...
                ht)
            (#,(if list>=vector #'list #'vector) init* ...)))])))]

This case study has demonstrated that our approach is general enough to
implement tools that provide recommendations that lead to asymptotic
improvements to source code, and powerful enough to improve upon such
tools by allowing the programmer to opt-in to automated changes without
changing their source code.
