#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure
   racket/port)

@title[#:tag "examples"]{Examples}
This section demonstrates how to use our mechanism, and how it
generalizes and advances past work on profile-guided meta-programs. We
first demonstrate optimizing Scheme's @racket[case] construct, a
multi-way branching constract similar to C's @code{switch}. Then
we then demonstrates profile-guided receiver class
prediction@~citea{grove95} for an object-oriented DSL. Finally
we demonstrate how our mechanism is powerful enough to reimplment
Perflint@~citea{liu09}. We provide list and vector libraries that warn
programmers when they may be using a less than optimal data structure,
and even provide a version that makes the choice automatically, based
on profile information. Complete versions of all examples are freely
available at @~cite[code-repo]. Racket implementations exist for all
examples for those without access to Chez Scheme.

@section{Scheme macro example}
Our mechanism and examples are implemented in Scheme, so we give below
a simple example to introduce Scheme meta-programming and its syntax.
@figure-here["sample-macro" "Sample macro"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK
;; Defines a macro (meta-program) `do-n-times'
;; Example:
;; (do-n-times 3 (display “*”)) expands into
;; (begin (display "*")
;;        (display "*")
;;        (display "*"))
(define-syntax (do-n-times stx)
  ;; pattern matches on the inputs syntax
  (syntax-case stx ()
    [(do-n-times n body)
     ;; Start generating code
     #`(begin
        ;; Runs at compile time then
        ;; splices the result into the
        ;; generated code
        #,@(let l [(i (syntax->datum n))]
             ;; Loops from n to 0
             (if (zero? i)
                 '()
                 ;; Create a list #'body
                 (cons #'body (l (sub1 i))))))])))]

The meta-program in @figure-ref{sample-macro} expects a number
@racket[n] and an expression @racket[body] and duplicates the expression
@racket[n] times. Each meta-program, created by @racket[define-syntax],
takes a single piece of syntax as its argument. We use @racket[syntax-case]
to access the subforms of the syntax via pattern matching. @racketmetafont{#'},
@racketmetafont{#`}, and @racketmetafont{#,} implement Lisp's quote,
quasiquote, and unquote but on syntax instead of lists. In the example,
we run a loop at compile-time that generates a list with @racket[n]
copies of the syntax @racket[body], and then splice
(@racketmetafont|{#,@}|) the copies into the generated program.

@section[#:tag "eg-case"]{Profile-guided conditional branch optimization}
The .NET compiler feature value probes, which enable profile-guided
reordering of if/else and @code{switch} statements @~cite[.net]. As our
first example, we demonstrate that our mechanism can be used to easily
implement this optimization without the specialized support of value
probes. We will optimize Scheme's @racket[cond] and @racket[case]
constructs, which are similar to if/else and @code{switch} in other
languages.

The Scheme @racket[cond] construct is analogous to a series of if/else
if statements. The clauses of @racket[cond] are executed in order until
the left-hand side of a clause is true. If there is an @racket[else]
clause, the right-hand side of the @racket[else] clause is taken only
if no other clause's left-hand side is true. @Figure-ref{cond-example}
shows and example program using @racket[cond].
@figure-here["cond-example" (elem "An example using " @racket[cond])
@#reader scribble/comment-reader
(racketblock0
(define (fact n)
 (cond
  [(zero? n) 1]
  [(eq? n 5) 120] ; A very common case
  [else (* n (fact (sub1 n)))])))]

We introduce the @racket[exclusive-cond] construct,
@figure-ref{exclusive-cond}, as a similar conditional branching
construct, but one that expects all branches to be mutually exclusive.
When the branches are mutually exclusive we can safely reorder the
clauses to execute the most likely clauses first. While the compiler
cannot prove such a property in general, meta-programming allows the
programmer to encode this knowledge in their program and take advantage
of optimizations that would have otherwise been impossible.

@; How does exclusive-cond use profile information to implement cond
The @racket[exclusive-cond] macro rearranges clauses based on
the profiling information of the right-hand sides. Since the left-hand
sides are executed depending on the order, profiling information from
the left-hand side is not enough to determine which clause is executed
most often. The @racket[clause] structure stores the original syntax for
@racket[exclusive-cond] clause and the weighted profile count for that
clause.  Since a valid @racket[exclusive-cond] clause is also a valid
@racket[cond] clause, we copy the syntax and generate a new
@racket[cond] in which the clauses are sorted according to profile
weights. Of course the @racket[else] clause it is always last and is not
included when sorting the other clauses.

We use the function @racket[profile-query-weight] to access the profile
information. Given a source object or piece of syntax, it returns the
associated profile weight.
@todo{Ensure this is runnable}
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
@todo{Ensure this is runnable}
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

@section[#:tag "eg-virtual-call"]{Profile-guided receiver class prediction}
In this example we demonstrate how to implement profile-guided receiver
class prediction@~citea{grove95} for an object-oriented DSL implemented
in Scheme. We perform this optimization by taking advantage of the
@racket[exclusive-cond] construct we developed in the last section.

We borrow the following example from Grove et.  al.@~citea{grove95}. The
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
back to dynamic dispath, we can even use the profile information to stop
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
                   (let* ([method-ht (cdr (hashtable-ref classes class &undefined))]
                          [method-info (hashtable-ref method-ht (syntax->datum #'m) &undefined)])
                    (with-syntax
                      ([(arg* ...) (cadr method-info)] [(body body* ...) (cddr method-info)])
                      ;; Inline only methods that use more than 20% of the computation.
                      (if (> (profile-query-weight #'body) .2)
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

@section[#:tag "eg-datatype"]{Data Structure Specialization}
@; Motivate an example that normal compilers just can't do
The examples in @secref{eg-case} and @secref{eg-virtual-call} shows that
we can easily bring well-known optimizations up to the meta-level.
This enables the DSL writer to take advantage of traditional profile-guided
optimizations, and language writers to give programmer new tools to
encode their knowledge. While profile-guided meta-programming
enables such traditional optimizations, it also enables higher level
decisions normally done by the programmer.

Past work has used profile information to give programmer feedback when
they make suboptimal use of algorithms and data structures provided by
standard libraries@~citea{liu09}, but left it up to the programmer to
change the code. In this section we show our mechanism can not only
reimplement this work, but also automate the proposed changes. By giving
meta-programs access to profile information, the programmer can choose
to automatically generate optimized code by simply importing a library.

In this example, we provide implementations of lists and vectors (array)
that warn the programmer when they may be using a less optimal data
structure. The implementations provide wrappers around the standard list
and vector functions that introduce new source objects to profile the
uses of each new list and vector separately. Finally, we provide an
implementation of a sequence datatype that will automatically specialize
to a list or vector based on profiling information. Complete versions of
both Chez Scheme and Racket implementations of this code are freely
available at @~cite[code-repo].

@figure**["profile-list" "Implementation of profile list"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK
(meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-list"))
(meta define param* #'(current-profiled-list?  current-profiled-map
  current-profiled-car current-profiled-cdr current-profiled-cons
  current-profiled-list-ref current-profiled-length))
(define-syntax (list x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  ;; Defines all the list operations, giving profiled implementations
  (define op*
    (real:map
      (lambda (v src)
        (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
      '(real:list? real:map real:car real:cdr real:cons real:list-ref real:length)
      (real:list #f #f #f list-src list-src vector-src vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (profile-query-weight list-src) (profile-query-weight vector-src))
       (printf "WARNING: You should probably reimplement this list as a vector: ~a\n"
               x))
     (with-syntax ([(def* ...) op*]
                   [(name* ...) (generate-temporaries op*)]
                   [(param* ...) param*])
       #`(let ()
           (define name* def*) ...
           (make-list-rep (lambda () (param* name*) ...)
             (real:list init* ...))))])))]

@Figure-ref{profile-list} shows the implementation of the profiled list
constructor. This meta program has the same interface as the standard
Scheme list constructor; it takes an arbitrary number of elements and
returns a representation of a linked list. We repesent a list as a pair
of the underlying linked list and a function that the current swaps list
operations to the profiled version we just generated. That is, a call to
@racket[list?] on our list representation will first 

In our meta-program, however,
we do not simply build a linked list. Instead we first build new source
fresh source objects. We then generate wrappers around all the list
operations using the source objects so that uses of this list get
profiled using the newly generated source objects. Operations that are
fast on vectors get a different source object than operations that are
fast on lists. Before emitting the code for the list, and the wrapped
list operations, we check for profile information from a previous run
and warn the user if profiling information suggests they should be using
a vector. Note that this requires that source objects are generated
deterministically across runs. Finally, we output the wrapped operations
as the representation of the. 
@todo{Probably everything after this get deleted}
@; Introduce example
The example in @figure-ref{sequence-datatype} chooses between a list and
a vector using profile information. If the program uses @racket[seq-set!] and
@racket[seq-ref] operations more often than @racket[seq-rest]
and @racket[seq-cons], then the sequence is implemented using a
@racket[vector], otherwise using a @racket[list].

The last line of @figure-ref{sequence-datatype} demonstrates the usage
of the @racket[define-sequence-datatype] macro. In this example, a
sequence named @racket[seq1] is defined and initialized to contain
elements @racket[0], @racket[3], @racket[2], and @racket[5].

The macro defines new profiled version of the sequence operations and
defines a new instance of sequence. The profiled operations are
redefined for @emph{each} new sequence, creating fresh source objects,
for each seperate sequence. This ensures each instance of a sequence is
profiled and specialized seperately. Here we assume we can create fresh
source objects via the function @racket[make-fresh-source-obj!]. We
discuss its implementation in @secref{implementation}.
