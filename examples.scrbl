#lang scribble/sigplan
@(require "defs.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@(require racket/port)
@title[#:tag "examples" "Examples"]
This section presents several macros that use profiling information to
optimize the expanded code. The first example demonstrates unrolling
loops based on profile information. While loop unrolling can be done
with low level profile information, we discuss when it can be useful or
even necessary to do at the meta-programming level.  The second example
demonstrates reordering the clauses of a conditional branching structure
,called @racket[exclusive-cond], based on profile information.  The
final example demonstrates specializing a data structure based on
profile information. 

@section{Loop Unrolling}
Loop unrolling is a standard compiler optimization.  However, striking
a balance between code growth and execution speed when unrolling loops
is tricky.  Profile information can help the compiler focus on the most
executed loops. 

Profile directed loop unrolling can be done using low-level profile
information. However, loop unrolling at a low-level requires associating
loops with the low level profiled structures, such internal nodes or
even basic blocks, and cannot easily handle arbitrary recursive
functions. More importantly, with the rise of DSLs in `high productivity'
@; https://www.usenix.org/system/files/conference/hotpar12/hotpar12-final37.pdf
languages such as Python and Ruby, which lack sophisticated compilers,
implementing loop unrolling via meta-programming may be necessary to get
high performance loops in a DSL. 

@; Explain a basic let-loop
This loop example unrolls Scheme's named let @footnote{Strictly
speaking, we do not implement named let, since in loop unrolling macro,
the name is not assignable.}, as seen in @figure-ref{fact5}. This
defines a loop that runs for @racket[i=5] to @racket[i=0] computing
factorial of @racket[5]. This named let might normally be implemented
via a recursive function, as seen in @figure-ref{named-let-simple}. The
example in @figure-ref{fact5} would produce a recursive function
@racket[fact], and immediately call it on @racket[5].

@figure-here[
  "fact5"
  "The most executed program in all of computer science" 
@racketblock[
(let fact ([i 5])
  (if (zero? i)
      1
      (* n (fact (sub1 n)))))]]

@figure-here["named-let-simple"
        "a simple definition of a named let"
@racketblock[
(define-syntax let
  (syntax-rules ()
    [(_ name ([x e] ...) body1 body2 ...)
     ((letrec ([name (lambda (x ...) body1 body2 ...)])) e ...)]i
    #;[(_ ([x e] ...) body1 body2 ...)
     ((lambda (x ...) body1 body2 ...) e ...)]))
]]

@figure-here["named-let"
        "a macro that does profile directed loop unrolling"
@racketblock[#:escape srsly-unsyntax
(define-syntax named-let
  (lambda (x)
    (syntax-case x ()
      [(_ name ([x e] ...) b1 b2 ...)
       #`((letrec ([tmp (lambda (x ...)
            #,(let* ([profile-weight 
                       (or (profile-query-weight #'b1) 0)]
                     [unroll-limit 
                       (floor (* 3 profile-weight))])
                #`(define-syntax name
                    (let ([count #,unroll-limit]
                          [weight #,profile-weight])
                      (lambda (q)
                        (syntax-case q ()
                          [(_ enew (... ...))
                            (if (or (= count 0)
                                    (< weight .1))
                                #'(tmp enew (... ...))
                                (begin
                                  (set! count (- count 1))
                                  #'((lambda (x ...) b1 b2 ...) 
                                     enew (... ...))))])))))
             b1 b2 ...)])
            tmp)
          e ...)])))]]

@; Explain how to do a profile directed named let unrolling
@Figure-ref{named-let} defines a macro, @racket[named-let], that unrolls
the loop between 1 and 3 times, depending on profile information. At
compile time, the macro-expander runs @racket[(or (profile-query-weight
#'b1) 0)]. This asks the runtime for the profile information associated
with @racket[b1], the first expression in the body of the loop. Recall
that @racket[profile-query-weight] returns a value between 0 and 1 if
there is profile information for a piece of syntax, and false otherwise.
Using the profile weight, we calculate @racket[unroll-limit]. If the
profile weight is 1, meaning the expression is executed more than any
other expression during the profiled run, @racket[unroll-limit] is 3. If
the weight is 0, meaning the expression is never executed during the
profiled run, @racket[unroll-limit] is 0. Finally, @racket[named-let]
generates a macro called @racket[name], where name is the identifier
used in the source code, does the work of unrolling the loop up to
@racket[unroll-limit] times.

@; Explain multiple call sites
In fact, a named let defines a recursive function and immediately
calls it. While this can be used for simple loops, a named let may have
non-tail calls or even multiple recursive calls along different
branches. This macro does more than loop unrolling--it does recursive
function lining. A more clever macro could unroll each call site a
different number of times, depending on how many times that particular
call is executed. This would allow more fine grain control over code
growth. For brevity, we restrict the example and assume
@racket[named-let] is used as a simple loop. Each call site is unrolled
the same number of times.

Similar macros are easy to write for @racket[do] loops, and even
for @racket[letrec] to inline general recursive functions. 

@section{exclusive-cond}
@racket[cond] is a Scheme branching construct analogous to a series of
if/else if statements. @Figure-ref{cond-example} shows syntax of a 
@racket[cond] clause. The clauses of @racket[cond] are executed in order
until the left-hand side of a clause is true. If there is an
@racket[else] clause, the right-hand side of the @racket[else] clause is
taken only if no other clause's left-hand side is true.

@;@racketblock[#,(port->string (open-input-file "cond-all.ss"))]
@figure-here["cond-example" (elem "An example of " @racket[cond])
@racketblock[
(cond
  [p1 e1]
  [p2 e2]
  [p3 e3]
  [else ee])]]

The first clause has a test on the left-hand side and some expression on
the right-hand side. If the left-hand side evaluates to a true value,
then the right-hand side is executed.  
The second form passes the value of the left-hand side to the function
on the right-hand side only if the left-hand side evaluates to a true
value. In Scheme, any value that is not @racket[#f] is true, so this
can be used to post-process non-boolean true values.  
The third form simply returns the value of the left-hand side if it
evaluates to a true value. The last form is equivalent to the clause
@racket[(e => (lambda (x) x))].

@figure["exclusive-cond" 
        (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader 
(racketblock 
(define-syntax exclusive-cond
  (lambda (x)
    (define-record-type clause
      (nongenerative)
      (fields (immutable clause) (immutable count))
      (protocol
        (lambda (new)
          (lambda (e1 e2)
            (new e1 (or (profile-query-weight e2) 0))))))
    (define parse-clause
      (lambda (clause)
        (syntax-case clause (=>)
          ;;[(e0) (make-clause clause ???)]
          [(e0 => e1) (make-clause clause #'e1)]
          [(e0 e1 e2 ...) (make-clause clause #'e1)]
          [_ (syntax-error clause "invalid clause")])))
    (define (helper clause* els) 
      (define (sort-em clause*)
        (sort (lambda (cl1 cl2) 
                (> (clause-count cl1) (clause-count cl2))) 
          (map parse-clause clause*)))
      #`(cond
          #,@(map clause-clause (sort-em clause*))
          #,@(if els #`(,els) #'())))
    (syntax-case x (else)
      [(_ m1 ... (else e1 e2 ...)) (helper #'(m1 ...) #'(else e1 e2 ...))]
      [(_ m1 ...) (helper #'(m1 ...) #f)])))
)]

@; How does exclusive-cond use profile information to implement cond
The @racket[exclusive-cond] macro, @figure-ref{exclusive-cond}, shows an
implementation of @racket[cond] that will rearrange clauses based on
the profiling information of the right-hand sides. Since the left-hand
sides will be executed depending on the order of the clauses, profiling
information from the left-hand side is not enough to determine which
clause is true most often. Unfortunately, this means we
cannot @note{By manually hacking source objects, it may be possible
but would not be pretty.} implement the third syntax in
@figure-ref{cond-forms} which has only a left-hand side.

@; How are clauses parsed
In order to sort the clauses, all clauses are parsed before the code is
generated. @racket[exclusive-cond] first parses each clause into a
clause record.  The clause record stores the original syntax for the
clause and the weighted profile count for that clause. @todo{Maybe why
we pick an expression from the body of each clause here, instead of up
there} Since a valid @racket[exclusive-cond] clause is also a valid
@racket[cond] clause, the syntax is simply copied.

@;@todo{syntax or code?}

@; How are clauses emitted in order
After parsing each clause, the clause records are sorted by the profile
weight. Once sorted, a @racket[cond] expression is generated by
emitting each clause in sorted order. If an @racket[else] clause exists
then it is emitted as the final clause.

@figure-here["exclusive-cond-expansion"
        (elem "An example of " @racket[exclusive-cond] " and its expansion")
@#reader scribble/comment-reader 
(racketblock 
(exclusive-cond
  [(fixnum? n) e1] ;; e1 executed 3 times
  [(flonum? n) e2] ;; e2 executed 8 times
  [(bignum? n) e3] ;; e3 executed 5 times
  [else e4])
)
@#reader scribble/comment-reader 
(racketblock
(cond
  [(flonum? n) e2] ;; e2 executed 8 times
  [(bignum? n) e3] ;; e3 executed 5 times
  [(fixnum? n) e1] ;; e1 executed 3 times
  [else e4])
)]

@Figure-ref{exclusive-cond-expansion} shows an example of
@racket[exclusive-cond] and the code to which it expands. In this
example, we assume @racket[e1] is executed 3 times, @racket[e2] is
executed 8 times, and @racket[e3] is executed 5 times.

@subsection{case}
@; How does case work
@racket[case] is a pattern matching construct that is easily given
profile directed optimization by implementing it in terms of
@racket[exclusive-cond]. @racket[case] takes an expression
@racket[key-expr] and an arbitrary number of clauses, followed by an
optional @racket[else] clause. The left-hand side of each clause is a
list of constants. @racket[case] executes the right-hand side of the
first clause in which @racket[key-expr] is @racket[eqv?] to some element
of the left-hand. If @racket[key-expr] is not @racket[eqv?] to any
element of any left-hand side and an @racket[else] clause exists then
the right-hand side of the @racket[else] clause is executed.

@figure-here["case-example"
        (elem "An example of a " @racket[case] " expression")
@racketblock[
(case x
  [(1 2 3) e1]
  [(3 4 5) e2]
  [else e3])
]]

@Figure-ref{case-example} shows an example @racket[case] expression. If
@racket[x] is 1, 2, or 3, then @racket[e1] is executed. If @racket[x] is
4 or 5, then @racket[e2] is executed. Note that while 3 appears in
the second clause, if @racket[x] is 3 then @racket[e1] will be
evaluated. The first occurrence always take precedence. 

@; How are clauses parsed
Since @racket[case] permits clauses to have overlapping elements and uses
order to determine which branch to take, we must remove overlapping elements
before clauses can be reordered. Each clause is parsed into the set of
left-hand side keys and right-hand side bodies. Overlapping keys are
removed by keeping only the first instance of each key when processing
the clauses in the original order. After removing overlapping keys, an
@racket[exclusive-cond] is generated. 

@figure-here["case-expansion"
        (elem "The expansion of " @figure-ref{case-example})
@racketblock[
(exclusive-cond x
  [(memv x (1 2 3)) e1]
  [(memv x (4 5)) e2]
  [else e3])
]]

@Figure-ref{case-expansion} shows how the example @racket[case]
expression from @figure-ref{case-example} expands into
@racket[exclusive-cond]. Note the duplicate 3 in the second clause is
dropped to preserve ordering constraints from @racket[case].

@section{Data type Selection}
@; Motivate an example that normal compilers just can't do
The previous optimizations focus on low level changes that can improve
code performance. Reordering clauses of a @racket[cond] can improve
speed by maximizing straight-line code emitted later in the compiler.
Loop unrolling can reduce overhead associate with loops and maximize
straight-line code emitted later in the compiler. While profile directed
meta-programming enables more of such low level optimizations, it
also enables higher level decisions normally done by the programmer

@figure**["sequence-datatype"
        "a macro that defines a sequence datatype based on profile information"
@racketblock[ #:escape srsly-unsyntax
(define-syntax define-sequence-datatype
  (let ([ht (make-eq-hashtable)])
    (define args
      `((seq? . #'(x))
        (seq-map . #'(f s))
        (seq-first . #'(s))
        (seq-ref . #'(s n))
        (seq-set! . #'(s i obj))) )
    (define defs 
      `((make-seq . (,#'list . ,#'vector))
        (seq? . (,#'list? . ,#'vector?))
        (seq-map . (,#'map . ,#'for-each))
        (seq-first . (,#'car . ,#'(lambda (x) (vector-ref x 0))))
        (seq-ref . (,#'list-ref . ,#'vector-ref))
        (seq-set! . (,#'(lambda (ls n obj) (set-car! (list-tail ls n) obj)) . ,#'vector-set!))))
    (define (choose-args name)
      (cond 
        [(assq name defs) => cdr]
        [else (syntax-error name "invalid method:")]))
    (define (choose name)
      (let ([seq-set!-count (hashtable-ref ht 'seq-set! 0)]
            [seq-ref-count (hashtable-ref ht 'seq-ref 0)]
            [seq-first-count (hashtable-ref ht 'seq-first 0)]
            [seq-map-count (hashtable-ref ht 'seq-map 0)])
      (cond 
        [(assq name defs) => 
          (lambda (x)
            (let ([x (cdr x)])
              (if (> (+ seq-set!-count seq-ref-count) 
                     (+ seq-first-count seq-map-count))
                  (cdr x)
                  (car x))))]
        [else (syntax-error name "invalid method:")])))
    (lambda (x)
      (syntax-case x ()
        [(_ var (init* ...) name* ...)
         (for-each 
           (lambda (name) 
             (hashtable-set! ht name 
               (or (profile-query-weight name) 0)))
           (map syntax->datum #'(name* ...)))
         (with-syntax ([(body* ...) (map (lambda (name) (choose (syntax->datum name))) #'(name* ...))]
                       [(args* ...) (map (lambda (args) (choose-args (syntax->datum name))) #'(name* ...))])
           #`(begin (define (name* args* ...) (begin name* (body* args* ...))) ...
                    (define var (#,(choose 'make-seq) init* ...))))]))))
]]

@; Introduce example
Consider a program in which a sequence type is required but the
it is not obvious what should be used to implement the sequence. @todo{I
don't like the first sentence.}
The example in @figure-ref{sequence-datatype} chooses between a list and
a vector using profile information. If @racket[seq-set!] and
@racket[seq-ref] operations are used more often than @racket[seq-map]
and @racket[seq-first], then a @racket[vector] is used, otherwise a
@racket[list] is used.

@figure["seq1-example"
        "Use of the define-sequence-datatype macro"
@racketblock[
(define-sequence-datatype seq1 (0 3 2 5)
  seq? seq-map seq-first seq-ref seq-set!)
]]

@todo{To hell with this example. We need to break it up and make it slightly
more sensible to use. I hate to make it OO, but that would make it
scoping issues easier. Maybe move @racket[choose] and nonsense to an
appendix and just focus on the macro here.}
@; Discuss quirks in example implementation
@Figure-ref{seq1-example} demonstrates the usage of the
@racket[define-sequence-datatype] macro. In this example, a sequence
named @racket[seq1] is defined and initialized to contain elements
@racket[0], @racket[3], @racket[2], and @racket[5]. The macro requires
the function names 
The unique source information
attached to each function name is used to profile the operations of that
@emph{particular} sequence. The definitions of each operation evaluate
the name to ensure function inlining does not distort profile counts. A
clever compiler might try to throw out the effect-free reference to
@racket[name] in the body of each operation, so this implementation is
fragile.
