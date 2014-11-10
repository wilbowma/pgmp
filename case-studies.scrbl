#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure
   racket/port)

@title[#:tag "case-studies"]{Case Studies}
In this section we evaluate our approach.
We show it is general enough to implement and improve upon existing
profile-guided meta-programs.
We first demonstrate optimizing Scheme's @racket[case] construct, a
multi-way branching construct similar to C's @code{switch}.
Then we then demonstrates profile-guided receiver class
prediction@~citea{grove95} for an object-oriented DSL.
Finally we demonstrate that our approach is powerful enough to
reimplement and improve upon Perflint@~citea{liu09} by providing a list
and vector libraries that warn programmers when they may be using a less
than optimal data structure, based on profile information.
We provide implementations of all case studies in both Chez Scheme and
Racket.

@section[#:tag "study-case"]{Profile-guided conditional branch optimization}
The .NET compiler features value probes, which enable profile-guided
reordering of if/else and @code{switch} statements.
As our first case study, we optimize Scheme's @racket[cond] and
@racket[case] constructs, which are similar to if/else and @code{switch}
in other languages.
This demonstrates that our approach can be used to easily implement
this optimization without the specialized support of value probes.
It also demonstrates that our approach allows programmers to use
meta-programming to encode their knowledge of the program, enabling
optimizations that may have been otherwise impossible.

The Scheme @racket[cond] construct is analogous to a series of
if/else-if statements.
To execute a @racket[cond], we run the left-hand side of each clause
until some left-hand side evaluates to true.
When we find the first true clause, we run the right-hand side of that
clause, and ignore further clauses.
If there is an @racket[else] clause, we run the right-hand side of the
@racket[else] clause only if no other clause's left-hand side is true.
@Figure-ref{cond-example} shows an example program using @racket[cond].
@figure-here["cond-example" (elem "An example using " @racket[cond])
@#reader scribble/comment-reader
(racketblock0
(define (lex char)
 (cond
  [(is-whitespace? char) e1]
  [(is-open-parn? char) e1]
  [else (* n (fact (sub1 n)))])))]

We introduce the @racket[exclusive-cond] construct,
@figure-ref{exclusive-cond}, as a condition branch construct similar to
@racket[cond], but one that expects all branches to be mutually
exclusive.
When the branches are mutually exclusive we can safely reorder the
clauses to execute the most likely clauses first.
While the compiler cannot prove such a property in general,
meta-programming allows the programmer to encode this knowledge in their
program and take advantage of optimizations that would have otherwise
been impossible.

@; How does exclusive-cond use profile information to implement cond
The @racket[exclusive-cond] macro rearranges clauses based on
the profiling information of the right-hand sides.
Since the left-hand sides are executed depending on the order, profiling
information from the left-hand side cannot be used to determine which
clause is executed most often.
The @racket[clause] structure stores the original syntax for
@racket[exclusive-cond] clause and the weighted profile count for that
clause.
Recall that @racket[profile-query-weight] may return @racket[#f]. We
do not care to distinguish between 0 and @racket[#f], so we use 0
when if we there is no profile information.
Since a valid @racket[exclusive-cond] clause is also a valid
@racket[cond] clause, we copy the syntax and generate a new
@racket[cond] in which the clauses are sorted according to profile
weights.
The @racketmetafont|{#,@}| splices a list of syntax objects into a
syntax object.
Of course the @racket[else] clause is always last and is not included
when sorting the other clauses.
@figure**["exclusive-cond" (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader
(RACKETBLOCK0
(define-syntax (exclusive-cond x)
  (define-record-type clause (fields syn weight))
  (define (parse-clause clause)
    (syntax-case clause ()
      [(e0 e1 e2 ...) (make-clause clause (or (profile-query-weight #'e1) 0))]
      [_ (syntax-error clause "invalid clause")]))
  (define (sort-clauses clause*)
    (sort (lambda (cl1 cl2) (> (clause-weight cl1) (clause-weight cl2)))
     (map parse-clause clause*)))
  (define (reorder-cond clause* els?)
    #`(cond #,@(map clause-syn (sort-clauses clause*)) . #,els?))
  (syntax-case x (else)
    [(_ m1 ... (else e1 e2 ...)) (reorder-cond #'(m1 ...) #'([else e1 e2 ...]))]
    [(_ m1 ...) (reorder-cond #'(m1 ...) #'())])))]

The @racket[case] construct takes an expression @racket[key-expr] and an arbitrary
number of clauses, followed by an optional @racket[else] clause. The
left-hand side of each clause is a list of constants. @racket[case]
executes the right-hand side of the first clause in which
@racket[key-expr] is @racket[eqv?] to some element of the left-hand. If
@racket[key-expr] is not @racket[eqv?] to any element of any left-hand
side and an @racket[else] clause exists then the right-hand side of the
@racket[else] clause is executed. @Figure-ref{case-example} shows an
example @racket[case] expression. In this example, the programmer has a
spurious @racket[0] in the second clause which should never be matched
against, since the first clause will always match @racket[0].
@figure-here["case-example" (elem "An example using " @racket[case])
@(racketblock0
(define (fact n)
 (case n
  [(0) 1]
  [(0 5) 120]
  [else (* n (fact (sub1 n)))])))]

@; How are clauses parsed
@Figure-ref{case-impl} shows the full profile-guided implementation of
@racket[case] that sorts clauses by which is executed most often.  The
majority of the work is in @racket[trim-keys!], which removes duplicate
keys to ensure mutually exclusive clauses. We omit its definition for
brevity. Since @racket[case] permits clauses to have overlapping
elements and uses order to determine which branch to take, we must
remove overlapping elements before reordering clauses. We parse each
clause into the set of left-hand side keys and right-hand side bodies.
We remove overlapping keys by keeping only the first instance of each
key when processing the clauses in the original order. After removing
overlapping keys, we generate an @racket[exclusive-cond] expression.
@figure**["case-impl" (elem "Implementation of " @racket[case] " using "
    @racket[exclusive-cond])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (case x)
  (define (helper key-expr clause* els?)
    (define-record-type clause (fields (mutable keys) body))
    (define (parse-clause clause)
      (syntax-case clause ()
        [((k ...) e1 e2 ...) (make-clause #'(k ...) #'(e1 e2 ...))]
        [_ (syntax-error "invalid case clause" clause)]))
    (define (emit clause*)
      #`(let ([t #,key-expr])
          (exclusive-cond
            #,@(map (λ (cl) #`[(memv t '#,(clause-keys cl)) #,@(clause-body cl)]) clause*)
            . #,els?)))
    (let ([clause* (map parse-clause clause*)])
      (for-each trim-keys! clause*) (emit clause*)))
    (syntax-case x (else)
      [(_ e clause ... [else e1 e2 ...]) (helper #'e #'(clause ...) #'([else e1 e2 ...]))]
      [(_ e clause ...) (helper #'e #'(clause ...) #'())])))]
@;{
  (define (trim-keys! clause)
   (let ()
    (define ht (make-hashtable equal-hash equal?))
    (clause-keys-set! clause
     (let f ([keys (clause-keys clause)])
      (if (null? keys)
       '()
       (let* ([key (car keys)]
              [datum-key (syntax->datum key)])
        (if (hashtable-ref ht datum-key #f)
         (f (cdr keys))
         (begin
          (hashtable-set! ht datum-key #t)
          (cons key (f (cdr keys)))))))))))}

@Figure-ref{case-expansion} shows how the example @racket[case]
expression from @figure-ref{case-example} expands into
@racket[exclusive-cond]. Note the duplicate @racket[0] in the second
clause is dropped to preserve ordering constraints from @racket[case].
@figure-here["case-expansion"
        (elem "The expansion of " @figure-ref{case-example})
@(racketblock0
(define (fact n)
 (let ([x n])
  (exclusive-cond x
   [(memv x '(0)) 1]
   [(memv x '(5)) 120]
   [else (* n (fact (sub1 n)))]))))]

Finally, @figure-ref{final-case-expansion} show the result of
expanding @racket[exclusive-cond] in @figure-ref{case-expansion}.
In the final generated program, the most common case is checked first.
@figure-here["final-case-expansion"
        (elem "The expansion of " @figure-ref{case-expansion})
@(racketblock0
(define (fact n)
 (let ([x n])
  (cond x
   [(memv x '(5)) 120]
   [(memv x '(0)) 1]
   [else (* n (fact (sub1 n)))]))))]

@section[#:tag "study-virtual-call"]{Profile-guided receiver class prediction}
In this example implement profile-guided receiver class
prediction@~citea["holzle1994optimizing" "grove95"] for an object-oriented DSL implemented in
Scheme. We perform this optimization by taking advantage of the
@racket[exclusive-cond] construct we developed in the last section. This
case study demonstrates that our mechanism is both general enough to implement
well-known profile-guided optimizations, and powerful enough to provide
DSL writers with standard PGOs.

We borrow the following case study from Grove et. al.@~citea{grove95}. The
classes @racket[Square] and @racket[Circle] implement the method
@racket[area].  The naïve DSL compiler simply expands every method call
into a conditional checks for known instances of classes and inlines the
correct method bodies, as in @figure-ref{naive-method-inline}. We would
like to inline the common cases, but if there are many known classes the
conditional tests may be too expensive to make this worthwhile.
Furthermore, we would like to perform the tests in order according to
which is most likely to succeed.
@;@racketblock[#,(port->string (open-input-file "cond-all.ss"))]
@figure-here["naive-method-inline" "Generated receiver class prediction code."
@(racketblock0
(cond
 [(class-equal? obj Square)
  (* (field obj length) (field obj width))]
 [(class-equal? obj Circle)
  (* pi (sqr (field obj r)))]
 [else (method obj area)]))]

As we saw in the previous section, @racket[exclusive-cond] provides a
way to encode our high level knowledge of the program. In particular, we
know class equality tests are mutually exclusive and safe to reorder. We
can simply reimplement method calls using @racket[exclusive-cond]
instead of @racket[cond] to get profile-guided receiver class
prediction. To eliminate uncommon cases altogether and more quickly fall
back to dynamic dispatch, we can even use the profile information to stop
inlining after a certain threshold. This implementation is shown in
@figure-ref{method-inline}. In this example, we arbitrarily choose to
inline only methods that take up more than 20% of the computation.
@figure**["method-inline" "The implementation of method inlining."
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
; Programmer calls to obj.m(val* ...) expand to (method-inline obj m val* ...)
; Inline likely classes in most likely order
(define-syntax (method-inline syn)
 (syntax-case syn ()
  [(_ obj m val* ...)
  (with-syntax ([(this-val* ...) #'(obj val* ...)])
  ;; Create an exclusive-cond, since it knows how to optimize clauses
   #`(exclusive-cond
       #,@(filter values
            (map (lambda (class)
                   (let* ([method-ht (cdr (hashtable-ref classes class #f))]
                          [method-info (hashtable-ref method-ht (syntax->datum #'m) #f)])
                    (with-syntax
                      ([(arg* ...) (cadr method-info)] [(body body* ...) (cddr method-info)])
                      ;; Inline only methods that use more than 20% of the computation.
                      (if (> (or (profile-query-weight #'body) 0) .2)
                          #`[(class-equal? obj #,(datum->syntax #'obj class))
                             (let ([arg* this-val*] ...) body body* ...)]
                          #f))))
            (vector->list (hashtable-keys classes))))
       ;; Fall back to dynamic dispatch
       [else (method obj m val* ...)]))])))]

@figure["profile-guided-method-results"
        "Profile-guided Generated code and expansion"
@#reader scribble/comment-reader
(racketblock0
;; method call expands to ==>
(exclusive-cond
 [(class-equal? obj Square)
  ;; executed 2 times
  (* (field obj length) (field obj width))]
 [(class-equal? obj Circle)
  ;; executed 5 times
  (* pi (sqr (field obj r)))]
 [else (method obj "area")])
;; expands to ==>
(cond
 [(class-equal? obj Circle)
  ;; executed 5 times
  (* pi (sqr (field obj r)))]
 [(class-equal? obj Square)
  ;; executed 2 times
  (* (field obj length) (field obj width))]
 [else (method obj "area")]))]

@Figure-ref{profile-guided-method-results} shows how our receiver
class prediction example is optimized through @racket[exclusive-cond].
Again, the generated @racket[cond] will test for the common case first.

@section[#:tag "study-datatype"]{Data Structure Specialization}
@; Motivate an example that normal compilers just can't do
While profile-guided optimizations can provide important speeds up by
optimizing code paths, programmers can us profile information to
identify much higher level performance issues. For instance, profile
information can be used to figure out that a different algorithms or
data structures might be cause an asymptotic speed up.@~citea{liu09}
In this case study we show our mechanism is general enough to implement
this kind of profiling tool, and even go beyond it by automating the
recommendations.

We provide implementations of lists and vectors (array)
that warn the programmer when they may be using a less optimal data
structure. The implementations provide wrappers around the standard list
and vector functions that introduce new source objects to profile the
uses of each new list and vector separately. Finally, we provide an
implementation of a sequence datatype that will automatically specialize
to a list or vector based on profiling information. Complete versions of
both Chez Scheme and Racket implementations of this code are freely
available at @~cite[code-repo].
@figure**["profile-list" "Implementation of profiled list"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK
(define-record list-rep (op-table ls))
(define (car ls)
  (make-list-rep (list-rep-op-table ls)
    ((hashtable-ref (list-rep-op-table ls) 'car #f)
     (list-rep-ls ls))))
...
(meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-list"))
(define-syntax (list x)
    ;; Create fresh source object. list-src profiles operations that are
    ;; fast on lists, and vector-src profiles operations that are fast on
    ;; vectors.
    (define list-src (make-fresh-source-obj! x))
    (define vector-src (make-fresh-source-obj! x))
    ;; Defines all the sequences operations, giving profiled implementations
    (define op-name* '(list? map car cdr cons list-ref length))
    (define op*
      (real:map
        (lambda (v src)
          (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
        '(real:list? real:map real:car real:cdr real:cons real:list-ref real:length)
        (real:list #f #f #f list-src list-src vector-src vector-src)))
    (syntax-case x ()
      [(_ init* ...)
       (unless (>= (or (profile-query-weight list-src) 0)
                   (or (profile-query-weight vector-src) 0))
         (printf "WARNING: You should probably reimplement this list as a vector: ~a\n" x))
        #`(let ()
            (make-list-rep
              (let ([ht (make-eq-hashtable)])
                #,@(real:map (lambda (op op-name) #`(hashtable-set! ht #,op-name #,op))
                     (syntax->list op*) (syntax->list op-name*))
                ht)
              (real:list init* ...)))])))]

@Figure-ref{profile-list} shows the implementation of the profiled list
constructor. This constructor has the same interface as the standard
Scheme list constructor---it takes an arbitrary number of elements and
returns a representation of a linked list. We represent a list as a pair
of the underlying linked list and a hash table of profiled list
operations. We generate these profiled operations by simply wrapping
calls to underlying, `real', list operations with freshly generated
source objects. We generate two source objects for each list.  One is
used to profile operations that are fast for lists and the other is used
to profile operations that are fast for vectors. Finally, we export new
versions of all the list operations that work on our new list
representation. For instance, @racket[car] takes our profiled list
representations, and calls the profiled version of @racket[car] from
the hash table of the profiled list on the underlying list. When
profiling information already exists, for instance, after a profiled
run, this list constructor emits a warning (at compile time) if the list
fast vector operations are more common than fast list operations.

We also provide an analogous implementation of vectors. While we
implement only two data structures here, this technique should scale to
the many other data structures analyzed by Perflint. However, we can do
one step better. Since our meta programs are integrated into the
language, rather than existing as a separate tool in front of the
compiler, we can provide libraries to the programmer that automatically
follow these recommendations rather than asking the programmer to change
their code. To demonstrate this, we implement a profiled sequence data
type that will automatically specialize to a list or vector, at compile
time, based on profile information.

@Figure-ref{profile-seq} shows the implementation of the profiled
sequence constructor. The code follows exactly the same pattern as the
profiled list. The key difference is we conditionally generate wrapped
versions of the list @emph{or} vector operations, and represent the
underlying data using a list @emph{or} vector, depending on the profile
information.
@figure**["profile-seq" "Implementation of profiled sequence"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK
(define-record seq-rep (op-table s))
...
(meta define make-fresh-source-obj! (make-fresh-source-obj-factory!  "profiled-seq"))
(define-syntax (seq x)
   (define list-src (make-fresh-source-obj! x))
   (define vector-src (make-fresh-source-obj! x))
   (define previous-list-usage (or (profile-query-weight list-src) 0))
   (define previous-vector-usage (or (profile-query-weight vector-src) 0))
   (define list>=vector (>= previous-list-usage previous-vector-usage))
   (define op-name* '(seq? seq-map seq-first seq-rest seq-cons seq-append
     seq-copy seq-ref seq-set! seq-length))
   (define op*
     (map
       (lambda (v src)
         (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
       (if list>=vector
           '(list? map first rest cons append list-copy list-ref
             list-set! length)
           '(vector? vector-map vector-first vector-rest vector-cons
             vector-append vector-copy vector-ref vector-set!
             vector-length))
       (list #f #f #f list-src list-src list-src #f vector-src vector-src
             vector-src)))
   (syntax-case x ()
     [(_ init* ...)
      #`(let ()
          (make-seq-rep
            (let ([ht (make-eq-hashtable)])
                #,@(map (lambda (op op-name) #`(hashtable-set! ht #,op-name #,op))
                     (syntax->list op*) (syntax->list op-name*))
                ht)
            (#,(if list>=vector #'list #'vector) init* ...)))])))]

This implementation of an automatically specializing data structure is
not ideal. The extra indirects through a hashtable and wrapped
operations introduce constant overhead to constructing a sequence, and
to every operation on the sequence. This case study does, however,
demonstrate that our mechanism is general and powerful enough to
implement novel profile directed optimizations.
