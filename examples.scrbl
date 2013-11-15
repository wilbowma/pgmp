#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require racket/port)
@title[#:tag "examples"]{Examples}
This section demonstrates how to use our mechanism, and how it
generalizes and advances past work on profile-guided meta-programs.  The
first example demonstrates profile-guided receiver class
prediction@~citea{grove95} for an object-oriented DSL based on profile
information. We then reuse part of that meta-program to optimize a
tokenizer. The final example demonstrates specializing a data
structure based on profile information.

@section{Scheme macro example}
Our system and examples are implemented in Scheme, so we give below
a simple example to briefly introduce Scheme meta-programming and
its syntax.

@figure-here["sample-macro" "Sample macro"
@#reader scribble/COMMENT-READER-T
@(RACKETBLOCK
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
         #,@(let loop [(i (syntax->datum n))] 
              ;; Loops from n to 0
              (if (zero? i)
                  '()
                   ;; Create a list #'body 
                   (cons #'body (loop (sub1 i))))))]))
)]

The meta-program in @figure-ref{sample-macro} expects a number
@racket[n] and an expression @racket[body] and duplicates the expression
@racket[n] times. Each meta-program, created by @racket[define-syntax],
takes a single piece of syntax as its argument. We use @racket[syntax-case]
to perform pattern matches on the syntax. @racketmetafont{#'},
@racketmetafont{#`}, and @racketmetafont{#,} implement Lisp's quote,
quasiquote, and unquote but on syntax instead of lists. In the example,
we run a loop at compile-time that generates a list with @racket[n]
copies of the syntax @racket[body], and then splice
(@racketmetafont["#,@"]) the copies into the generated program. 

@section[#:tag "eg-virtual-call"]{Profile-guided receiver class prediction}
In this example we demonstrate how to implement profile-guided receiver
class prediction@~citea{grove95} for a hypothetical object-oriented DSL
with virtual methods, similar to C++. We perform this optimization
through a general meta-program called @racket[exclusive-cond], a
branching construct that can automatically reorder its clauses based on
which is most likely to be executed. 

@racket[cond] is a Scheme branching construct analogous to a series of
if/else if statements. The clauses of
@racket[cond] are executed in order until the left-hand side of a clause
is true. If there is an @racket[else] clause, the right-hand side of the
@racket[else] clause is taken only if no other clause's left-hand side
is true.

@Figure-ref{cond-example} shows an example of a @racket[cond] generated
by our hypothetical OO DSL. We assume the DSL compiler simply expands
every virtual method call into a conditional branch for known instances
of objects and relies on another meta-program to reorder branches and
throw out uncommon cases.

We borrow the following example from Grove et. al.@~citea{grove95}.
Consider a class @racket[Shape] with a virtual method @racket[area].
@racket[Square] and @racket[Circle] inherit @racket[Shape]
and implement the virtual @racket[area]. We will use
@racket[exclusive-cond] to reorder inlined virtual method calls to
optimize the common case, and fall back to dynamic virtual method
dispatch.

@;@racketblock[#,(port->string (open-input-file "cond-all.ss"))]
@figure-here["cond-example" (elem "An example of " @racket[cond])
@racketblock[
(cond
 [(class-equal? obj Square) 
  (* (field obj length) (field obj width))]
 [(class-equal? obj Circle) 
  (* pi (sqr (field obj r)))]
 [else (method obj "area")])]]

By profiling the branches of the @racket[cond], we can sort the clauses
in order of most likely to succeed. However, @racket[cond] is order
dependent. While the programmer can see the clauses are mutually
exclusive, the compiler cannot prove this in general and cannot reorder
the clauses. 

Instead of cursing Rice's theorem, we use meta-programming
to encode and take advantage of this high-level knowledge. We define
@racket[exclusive-cond], @figure-ref{exclusive-cond}, with the same
syntax and semantics of @racket[cond] @note{Schemers: we omit the
alternative cond syntaxes for brevity.}, but without the specific order
of execution. We then use profile information to reorder the clauses.

@figure**["exclusive-cond" 
        (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader 
(RACKETBLOCK 
(define-syntax (exclusive-cond x)
  (define-record-type clause (fields syn weight))
  (define (parse-clause clause)
    (syntax-case clause ()
      [(e0 e1 e2 ...) (make-clause clause (or (profile-query-weight #'e1) 0))]
      [_ (syntax-error clause "invalid clause")]))
  (define (sort-clauses clause*)
    (sort (lambda (cl1 cl2) 
            (> (clause-weight cl1) (clause-weight cl2))) 
     (map parse-clause clause*)))
  (define (reorder-cond clause* els?) 
    #`(cond
        #,@(map clause-syn (sort-clauses clause*)) . #,els?))
  (syntax-case x (else)
    [(_ m1 ... (else e1 e2 ...)) (reorder-cond #'(m1 ...) #'([else e1 e2 ...]))]
    [(_ m1 ...) (reorder-cond #'(m1 ...) #'())]))
)]

@; How does exclusive-cond use profile information to implement cond
The @racket[exclusive-cond] macro rearranges clauses based on
the profiling information of the right-hand sides. Since the left-hand
sides are executed depending on the order of the clauses, profiling
information from the left-hand side is not enough to determine which
clause is executed most often. The @racket[clause] structure stores the
original syntax for @racket[exclusive-cond] clause and the weighted
profile count for that clause.  Since a valid @racket[exclusive-cond]
clause is also a valid @racket[cond] clause, we copy the syntax 
and generate a new @racket[cond] with the clauses sorted according to
profile weights. Of course we do not include the @racket[else] clause
when reordering other clauses; it is always last.

We use the function @racket[profile-query-weight] to access the profile
information. Given a source object or source expression, it returns the
associated profile weight.

@figure-here["exclusive-cond-expansion"
        (elem "An example of " @racket[exclusive-cond] " and its expansion")
@#reader scribble/comment-reader 
(racketblock
(exclusive-cond
 [(class-equal? obj Square) 
  ;; executed 2 times
  (* (field obj length) (field obj width))]
 [(class-equal? obj Circle) 
  ;; executed 5 times
  (* pi (sqr (field obj r)))]
 [else (method obj "area")]))

@#reader scribble/comment-reader 
(racketblock
(cond
 [(class-equal? obj Circle) 
  ;; executed 5 times
  (* pi (sqr (field obj r)))]
 [(class-equal? obj Square) 
  ;; executed 2 times
  (* (field obj length) (field obj width))]
 [else (method obj "area")])
)]

@Figure-ref{exclusive-cond-expansion} shows how our receiver
class prediction example is optimized through 
@racket[exclusive-cond]. The generated @racket[cond] will test for
@racket[Circle] (the common case) first.

@section[#:tag "eg-case"]{Fast Path Tokenizer}
In this example we demonstrate how to use the general meta-program,
@racket[exclusive-cond], presented in the previous example to optimize a
tokenizer. A tokenizer in Scheme can be written naturally
using @racket[cond] or @racket[case], a pattern matching construct
similar to C's @racket[switch]. Such a tokenizercan be easily
optimized by using the @racket[exclusive-cond] macro we saw earlier. 

@racket[case] takes an expression @racket[key-expr] and an arbitrary
number of clauses, followed by an optional @racket[else] clause. The
left-hand side of each clause is a list of constants. @racket[case]
executes the right-hand side of the first clause in which
@racket[key-expr] is @racket[eqv?] to some element of the left-hand. If
@racket[key-expr] is not @racket[eqv?] to any element of any left-hand
side and an @racket[else] clause exists then the right-hand side of the
@racket[else] clause is executed.

@figure-here["case-example"
        (elem "An example tokenizer using " @racket[case])
@racketblock[
  (case (read-token)
        [(#\space) e1]
        [(#\)) e2]
        [(#\( #\)) e3] 
        ...
        [else e-else])
]]

@Figure-ref{case-example} shows an example @racket[case] expression. If
the token is a space, @racket[e1] is executed. If the token is a right
paren then @racket[e2] is executed. If the token is a left paren then
@racket[e3] is executed. If no other clauses match, then @racket[e-else]
is executed. Note that the third clause has an extra right paren
character that can never be reached, since it would first match the
second clause.

@; How are clauses parsed
@Figure-ref{case-impl} shows the full implementation of case. The
majority of the work is in @racket[trim-keys!], which removes duplicate
keys to ensure mutually exclusive clauses. Since @racket[case] permits
clauses to have overlapping elements and uses order to determine which
branch to take, we must remove overlapping elements before reordering
clauses. We parse each clause into the set of left-hand side
keys and right-hand side bodies. We remove overlapping keys
by keeping only the first instance of each key when processing the
clauses in the original order.  After removing overlapping keys, we
generate an @racket[exclusive-cond].

@figure**["case-impl" (elem "Implementation of " @racket[case] " using "
@racket[exclusive-cond])
@#reader scribble/COMMENT-READER-T 
(RACKETBLOCK 
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
            #,@(map (lambda (clause) 
                      #`[(memv t '#,(clause-keys clause)) 
                         #,@(clause-body clause)])
                    clause*)
            . #,els?)))
    (let ([clause* (map parse-clause clause*)])
       (define ht (make-hashtable equal-hash equal?))
       (define (trim-keys! clause)
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
                          (cons key (f (cdr keys))))))))))
                (for-each trim-keys! clause*)
                (emit clause*)))
    (syntax-case x (else)
      [(_ e clause ... [else e1 e2 ...])
       (helper #'e #'(clause ...) #'([else e1 e2 ...]))]
      [(_ e clause ...)
       (helper #'e #'(clause ...) #'())]))
)]
@figure-here["case-expansion"
        (elem "The expansion of " @figure-ref{case-example})
@racketblock[
(let ([x (read-token)])
  (exclusive-cond 
    [(memv x '(#\space)) e1]
    [(memv x '(#\))) e2]
    [(memv x '(#\()) e3]
    ...
    [else e-else]))
]]

@Figure-ref{case-expansion} shows how the example @racket[case]
expression from @figure-ref{case-example} expands into
@racket[exclusive-cond]. Note the duplicate right paren in the third
clause is dropped to preserve ordering constraints from @racket[case].

@section[#:tag "eg-datatype"]{Data Structure Specialization}
@; Motivate an example that normal compilers just can't do
The example in @secref{eg-virtual-call} shows that we can easily bring
well-known optimizations up to the meta-level, enabling the DSL writer
to take advantage of traditional profile-guided optimizations.  While
profile-guided meta-programming enables such traditional optimizations,
it also enables higher level decisions normally done by the programmer.

Past work has used profile information to give programmer feedback when
they make suboptimal use of algorithms and data structures provided by
standard libraries@~citea{liu09}, but left it up to the programmer to
change the code. Our mechanism enables automating these changes. By 
giving meta-programs access to profile information we can automatically
generate optimized code. 

In this example, we provide an abstract sequence data structure that
changes its implementation based on profile information. This simple
example defaults to a list, but specializes to a vector (array) when
vector operations are more common than list operations. While
simplified, this example shows that our mechanism support making
high-level decisions normally left to the programmer.

@figure**["sequence-datatype"
          (elem "Implementation of " @racket[define-sequence-datatype])
@#reader scribble/COMMENT-READER-T 
(RACKETBLOCK 
(define-syntax (define-sequence-datatype x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define list-src (make-fresh-source-obj!))
  (define vector-src (make-fresh-source-obj!))
  ;; Defines all the sequences operations, giving implementations for
  ;; lists and vectors. 
  (define op*
    `((make-seq ,#'list ,#'vector)
      (seq? ,#'list? ,#'vector?)
      (seq-map ,#'map ,#'vector-map)
      (seq-first ,#'first ,#'(lambda (x) (vector-ref x 0)))
      ;; Wrap the operations we care about with a profile form
      (seq-rest ,#`(lambda (ls) (profile #,list-src) (rest ls))
                ,#`(lambda (v) 
                     (profile #,list-src)
                     (let ([i 1]
                           [v-new (make-vector (sub1 (vector-length v)))])
                       (vector-for-each 
                         (lambda (x) 
                           (vector-set! v-new i x)
                           (set! i (add1 i))) 
                         v))))
      (seq-cons ,#`(lambda (x ls) (profile #,list-src) (cons x ls)) 
                ,#`(lambda (x v) 
                     (profile #,list-src)
                     (let ([i 0]
                           [v-new (make-vector (add1 (vector-length v)))])
                       (vector-for-each 
                         (lambda (x) 
                           (vector-set! v-new i x)
                           (set! i (add1 i))) 
                         v))))
      (seq-ref ,#`(lambda (ls n) (profile #,vector-src) (list-ref ls n))
               ,#`(lambda (v n) (profile #,vector-src (vector-ref v n))))
      (seq-set! ,#`(lambda (ls n obj) 
                     (profile #,vector-src) (set-car! (list-tail ls n) obj)
                ,#`(lambda (v n obj) 
                     (profile #,vector-src) (vector-set! v n obj))))))
    ;; Default to list; switch to vector when profile information
    ;; suggests we should.
    (define (choose-op name)
      ((if (> (profile-query-weight vector-src)
              (profile-query-weight list-src))
          third
          second)
       (assq name op*)))
  (syntax-case x ()
    [(_ var (init* ...))
     ;; Create lists of syntax for operation names and definitions
     (with-syntax ([(name* ...) (map first op*)]
                   [(def* ...) (map choose (map first op*))])
       ;; and generate them
       #`(begin (define name* def*) ...
       ;; Finally, bind the sequence.
                (define var (#,(choose 'make-seq) init* ...))))]))

;; Define an abstract sequence
(define-sequence-datatype seq1 (0 3 2 5))
)]

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
