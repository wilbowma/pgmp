#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/footnote
   scriblib/figure)

@title[#:tag "study-case"]{Profile-guided conditional branch optimization}
As our first case study, we perform profile-guided optimization of
Scheme's @racket[case] construct, which is similar to C's @code{switch}
statement.
In C#, @code{switch} statements must be mutually exclusive and do not
allow fall through---each case must end in a jump such as @code{break}.
The .NET compiler features a profile-guided optimization of @code{switch}
statements that uses profile information to reorder the branches
according to which branch is most likely to succeed.
This case study shows that our approach is general enough to implement
this optimization.
The entire implementation is 65-line.

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
The full implementation of @racket[case] @nonbreaking{is 41-line.}
@figure-here["case-example" (elem "An example using " @racket[case])
@(racketblock0
(define (parse stream)
 (case (peek-char stream)
  [(#\space #\tab) (white-space stream)]
  [(0 1 2 3 4 5 6 7 8 9) (digit stream)]
  [(#\() (start-paren stream)]
  [(#\)) (end-paren stream)]
  ....)))]

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
mutually exclusive, in order to take advantage of optimizations
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
expression from @Figure-ref{case-example}
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

This case study demonstrates that our approach is general enough to
implement a well-known PGO via meta-programming. 
It also demonstrates some important features of our approach.
Namely, that through our approach new PGOs can easily be defined in
terms of existing ones, and new optimizations can be exposed when
programmers can encode domain specific knowledge into @nonbreaking{the program.}
