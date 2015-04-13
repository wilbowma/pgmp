#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure)

@title[#:tag "study-case"]{Profile-guided conditional branch optimization}
In C#, @code{switch} statements must be mutually exclusive and do not
allow fall through---each case must end in a jump such as @code{break}.
The .NET compiler features a profile-guided optimization of @code{switch}
statements that uses profile information to reorder the branches
according to which branch is most likely to succeed.

In this section, we describe a similar optimizaton for Scheme and
Racket @racket[case] expressions.
The implementation is straightforward and just 81 lines long.
More importantly, it is not baked into the compiler and can be
adapted to other forms of conditional expressions without changes to
the underlying compiler.

The @racket[case] expression takes an expression @racket[key-expr] and an
arbitrary number of clauses, followed by an optional @racket[else]
clause.
Each clause consists of a list of constants on the left-hand side and a
body expression on the right-hand side.
A @racket[case] expression executes the body of the first clause in which
@racket[key-expr] is @racket[equal?] to some element of the left-hand
side.
For simplicity, we present a version of @racket[case] that assumes that
a constant does not appear in the left-hand side of more than one
clause and does not support an @racket[else] clause@note{The full
implementation handles the full generality of Scheme's @racket[case]}.
@Figure-ref{case-example} shows an example @racket[case] expression.

@; How are clauses parsed
@Figure-ref{case-impl} shows the profile-guided implementation of
@racket[case] that reorders branches according to which clause is most
likely to succeed.
It creates an invocation of another meta-program,
@racket[exclusive-cond], which reorders its branches based on profile
information.
The implementation rewrites each @racket[case] clause into an
@racket[exclusive-cond] clause.
The form @RACKET[#,\@] splices the list of rewritten clauses into the
template for the @racket[exclusive-cond] expression.
An @racket[exclusive-cond] clause consists of a boolean expression on
the left-hand side and a body expression on the right-hand side.
Each @racket[case] clause is transformed by converting the left-hand
side into an explicit membership test for @racket[key-expr], while
leaving the body unchanged.
The full implementation of @racket[case] in Racket, which also removes
duplicate constants from successive clauses and supports an optional
@racket[else] clause that is never reordered, is 50 lines long.
@figure-here["case-example" (elem "An example using " @racket[case])
@(racketblock0
(define (parse stream)
 (case (peek-char stream)
  [(#\space #\tab) (white-space stream)]
  [(0 1 2 3 4 5 6 7 8 9) (digit stream)]
  [(#\() (start-paren stream)]
  [(#\)) (end-paren stream)]
  ....)))]

@figure-here["case-impl" (elem "Implementation of " @racket[case])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (case syn)
 ; Internal definition
 (define (rewrite-clause key-expr clause)
  (syntax-case clause ()
   [((k ...) body)
    ; Take this branch if the key expression
    ; is a equal? to some element of the list
    ; of constants
    #`((key-in? #,key-expr '(k ...)) body)]))
 ; Start of code transformation.
 (syntax-case syn ()
  [(_ key-expr clause ...)
   ; Evaluate the key-expr only once, instead of
   ; copying the entire expression in the
   ; template.
   #`(let ([t key-expr])
       (exclusive-cond
        ; transform each @racket[case] clauses
        ; into an @racket[exclusive-cond] clause
        #,@(map (curry rewrite-clause #'key-expr)
                #'(clause ...))))])))]

@Figure-ref{exclusive-cond} shows the implementation of the
@racket[exclusive-cond] expression.
This is a multi-way conditional branch similar to Lisp's @racket[cond],
except that all branches must be mutually exclusive.
Because the branches are mutually exclusive, @racket[exclusive-cond] can
safely reorder them.
The implementation of @racket[exclusive-cond] simply sorts each clause
by profile weight and generates a regular @racket[cond].
Since each @racket[exclusive-cond] clause is also a @racket[cond]
clause, the clauses do not need to be transformed.
@Figure-ref{case-expansion} shows the code generated after expanding
@racket[case] and then after expanding @racket[exclusive-cond] 
in the example @racket[case] expression in @Figure-ref{case-example}.
The full implementation of @racket[exclusive-cond] in Racket,
which also handles additional @racket[cond] syntaxes and an optional
@racket[else] clause that is never reordered, is 31 lines long.

Separating the implementation of @racket[exclusive-cond] and
@racket[case] in this way simplifies the implementation
of @racket[case].
The @racket[exclusive-cond] expression also demonstrates an important
feature of profile-guided meta-programming---meta-programming allows the
programmer to encode their domain-specific knowledge, e.g., that the
branches of this conditional are mutually exclusive, in order to take
advantage of optimizations that would have otherwise @nonbreaking{been impossible.}
@figure-here["exclusive-cond" (elem "Implementation of " @racket[exclusive-cond])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (exclusive-cond syn)
  ; Internal definitions
  (define (clause-weight clause)
    (syntax-case clause ()
      [(test e1 e2 ...) (profile-query #'e1)]))
  (define (sort-clauses clause*)
    ; Sort clauses greatest-to-least by weight
    (sort clause* > #:key clause-weight))
  ; Start of code transformation
  (syntax-case x ()
    [(_ clause ...)
     ; Splice sorted clauses into a @racket[cond]
     ; expression
     #`(cond #,@(sort-clause #'(clause ...)))])))]

@figure-here["case-expansion"
        (elem "Generated code from " @Figure-ref{case-example})
@#reader scribble/comment-reader
@(racketblock0
;; After case expands
(define (parse stream)
 (let ([t (peek-char stream)])
   (exclusive-cond
     [(key-in? t '(#\space #\tab))
      (white-space stream)]
     [(key-in? t '(0 1 2 3 4 5 6 7 8 9))
      (digit stream)]
     [(key-in? t '(#\()) (start-paren stream)]
     [(key-in? t '(#\))) (end-paren stream)]
     ....
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
     ....
     ))))]
