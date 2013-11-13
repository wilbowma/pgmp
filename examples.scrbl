#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require racket/port)
@title[#:tag "examples" "Examples"]
This section demonstrates how our to use our mechanism, and how it
generalizes and advances past work on profile-guided meta-programs.  The
first example demonstrates profile-guided receiver class prediction for
a object-oriented DSL based on profile information.  The final example
demonstrates specializing a data structure based on profile information.

@section{Scheme macro primer}
@figure-here["Sample macro" (elem "Sample macro")
@#reader scribble/comment-reader
@(racketblock
;; defines a macro (meta-program)
(define-syntax (do-n-times stx)
  ;; pattern matches on the inputs syntax
  (syntax-case stx ()
    ;; Example:
    ;; (do-n-times 5 (display “*”)) => *****
    [(do-n-times n body …)
    ;; #' creates a piece of syntax 
     #'(let loop [(i n)]  ;; the syntax `n', taken as input, is copied
              (if (zero? i)
                  (void)
                  (begin body … (loop (sub1 i)))))]))
)]

@tt{#'}, @tt{#`}, and @tt{#,} implement Lisp's quote,
quasiquote, and unquote but on syntax instead of lists. 
@todo{expand, fix tt}

@section{Profile-guided receiver class prediction}
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
by our hypothetical OO DSL. The DSL compiler simply expands every
virtual method call into a conditional branch for known instances of an
object.

We borrow the following example from Grove et. al.@~citea{grove95}.
Consider a class @racket[Shape] with a virtual method @racket[area].
@racket[Square] and @racket[Circle] inherit @racket[Shape]
and implement the virtual @racket[area]. We will use
@racket[exclusive-cond] to inline virtual method calls.

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
in order of most likely to succeed, or even drop clauses that occur too
infrequently inline. However, @racket[cond] is order dependent. While
the programmer can see the clauses are mutually exclusive, the compiler
cannot prove this in general and cannot reorder the clauses. 

Instead of wishing our compiler was more clever, we use meta-programming
to take advantage of this high-level knowledge. We define
@racket[exclusive-cond], @figure-ref{exclusive-cond}, with the same
syntax and semantics of @racket[cond] @note{Schemers: we omit the
alternative cond syntaxes for brevity.}, but with the restriction that
clause order is not guaranteed. We then use profile information to
reorder the clauses.

@figure**["exclusive-cond" 
        (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader 
(racketblock 
(define-syntax exclusive-cond
  (lambda (x)
    (define-record-type clause (fields syn weight))
    (define (parse-clause clause)
      (syntax-case clause ()
        [(e0 e1 e2 ...) (make-clause clause (or (profile-query-weight #'e1) 0))]
        [_ (syntax-error clause "invalid clause")]))
    (define (sort-clauses clause*)
      (sort (lambda (cl1 cl2) 
              (> (clause-weight cl1) (clause-weight cl2))) 
       (map parse-clause clause*)))
    (define (reorder-cond clause* els) 
      #`(cond
          #,@(map clause-syn (sort-clauses clause*))
          #,@(if els #`(,els) #'())))
    (syntax-case x (else)
      [(_ m1 ... (else e1 e2 ...)) (reorder-cond #'(m1 ...) #'(else e1 e2 ...))]
      [(_ m1 ...) (reorder-cond #'(m1 ...) #f)])))
)]

@; How does exclusive-cond use profile information to implement cond
The @racket[exclusive-cond] macro will rearrange clauses based on
the profiling information of the right-hand sides. Since the left-hand
sides will be executed depending on the order of the clauses, profiling
information from the left-hand side is not enough to determine which
clause is true most often. The clause record
stores the original syntax for the clause and the weighted profile count
for that clause. Since a valid @racket[exclusive-cond] clause is also a
valid @racket[cond] clause, the syntax is simply copied, and a new
@racket[cond] is generated with the clauses sorted according to profile
weights. If an @racket[else] clause exists then it is emitted as the
final clause.

@figure**["exclusive-cond-expansion"
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

@Figure-ref{exclusive-cond-expansion} shows an example of
@racket[exclusive-cond] and the code to which it expands. In this
example, we assume the object is a @racket[Circle] most of the time.  

@section{Fast Path Lexical Analyzer}
A lexical analyzer in Scheme can be written naturally using
@racket[cond] or @racket[case], a pattern matching construct similar to
C's @racket[switch]. Such a lexical analyzer can be easily optimized
using the @racket[exclusive-cond] macro we saw earlier. 

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
character that can never be reached, since it would first match teh
second clause.

@; How are clauses parsed
Since @racket[case] permits clauses to have overlapping elements and
uses order to determine which branch to take, we must remove overlapping
elements before clauses can be reordered. We parse each clause is parsed
into the set of left-hand side keys and right-hand side bodies. We
remove overlapping keys are removed by keeping only the first instance
of each key when processing the clauses in the original order. After
removing overlapping keys, an @racket[exclusive-cond] is generated. 

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

@section{Datatype Specialization}
@; Motivate an example that normal compilers just can't do
The previous examples show that we can easily bring well-known
optimizations up to the meta-level, enabling the DSL writer to take
advantage of traditional profile directed optimizations.
While profile directed meta-programming enables such traditional
optimizations, it also enables higher level decisions normally done by
the programmer.

In this example we present a library that provides a sequence datatype.
We consider this in the context of a DSL or library writer whose users
are domain experts, but not computer scientists. While a domain expert
writing a program my know they need a sequence for their program, they
may not have the knowledge to figure out if they should use a tree, or a
list, or a vector. Past work has bridge this gap in knowledge by
providing tools that can recommend changes and provide feedback
@todo{http://dx.doi.org/10.1109/CGO.2009.36}. We take this a step
further and provide a library that will automatically specialize the
data structure based on usage.

@figure**["sequence-datatype"
          (elem "Implementation of " @racket[define-sequence-datatype])
@#reader scribble/COMMENT-READER-T 
(RACKETBLOCK 
(define-syntax define-sequence-datatype
   (lambda (x)
      ;; Create fresh source object. list-src profiles operations that are
      ;; fast on lists, and vector-src profiles operations that are fast on
      ;; vectors.
      (define list-src (make-source-obj))
      (define vector-src (make-source-obj))
      ;; Defines all the sequences operations, giving implementations for
      ;; lists and vectors. 
      (define op*
        `((make-seq ,#'list ,#'vector)
          (seq? ,#'list? ,#'vector?)
          ;; Wrap the operations we care about with a profile form
          (seq-map ,#`(lambda (f ls) (profile #,list-src) (map f ls))
                   ,#`(lambda (f ls) (profile #,list-src) (vector-map f ls)))
          (seq-first ,#'first ,#'(lambda (x) (vector-ref x 0)))
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
                    (define var (#,(choose 'make-seq) init* ...))))])))
)]

@; Introduce example
The example in @figure-ref{sequence-datatype} chooses between a list and
a vector using profile information. If the program uses @racket[seq-set!] and
@racket[seq-ref] operations more often than @racket[seq-map]
and @racket[seq-first], then the sequence is implemented using a
@racket[vector], otherwise using a @racket[list].

@figure["seq1-example"
        "Use of the define-sequence-datatype macro"
@racketblock[
(define-sequence-datatype seq1 (0 3 2 5))
]]

@Figure-ref{seq1-example} demonstrates the usage of the
@racket[define-sequence-datatype] macro. In this example, a sequence
named @racket[seq1] is defined and initialized to contain elements
@racket[0], @racket[3], @racket[2], and @racket[5].

The macro expands into a series of definitions for each sequence
operations and a definition for the sequence datatype. This example
redefines the operations for each new sequence, creating fresh source
objects and new profiled operations for each seperate sequence. This
ensures each instance of a sequence is profiled seperately.
