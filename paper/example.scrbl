#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/figure
   scriblib/footnote)

@title[#:tag "example"]{A Running Example}
We first introduce a syntax extension to familiarize readers with Scheme
and Racket style meta-programming and to provide a running example.
This transformation presented is not a meaningful optimization and is
only used for illustrative purposes.
@figure-here["sample-macro" "Example syntax extension"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (if-r stx)
  (syntax-case stx ()
    [(if-r test t-branch f-branch)
     ; This @racket[let] expression runs at compile-time
     (let ([t-prof (profile-query #'t-branch)]
           [f-prof (profile-query #'f-branch)])
       ; This @racket[cond] expression runs at
       ; compile-time, and conditionally
       ; generates runtime code based on profile 
       ; information.
       (cond
          [(< t-prof f-prof)
          ; This @racket[if] expression would run at
          ; runtime when generated.
           #'(if (not test) f-branch t-branch)]
          [(>= t-prof f-prof)
          ; So would this @racket[if] expression.
           #'(if test t-branch f-branch)]))]))

; Example use of @racket[if-r]
(define (classify email)
  (if-r (subject-contains email "PLDI")
        (flag email 'important)
        (flag email 'spam)))

; Assuming profile information tells us:
;   @racket[(flag email 'important)] runs @racket[5] times
;   @racket[(flag email 'spam)]      runs @racket[10] times
; Then the above use of @racket[if-r] is rewritten to:
(define (classify email)
  (if (not (subject-contains email "PLDI"))
      (flag email 'spam)
      (flag email 'important))))]

In @Figure-ref{sample-macro},
@racket[define-syntax] introduces a new syntax extension @racket[if-r].
Any uses of @racket[if-r] in the source will be rewritten using the code
in the body of the extension. The syntax extension can be thought of as
a function from source expressions to @nonbreaking{source expression}.

When used in @Figure-ref{sample-macro}, the syntax extension
@racket[if-r] receives @nonbreaking{the argument:}
@racketblock0[
#'(if-r (subject-contains-ci email "PLDI")
        (flag email 'important)
        (flag email 'spam))]

This is a data representation of a term called a syntax object. The
forms @racketmetafont{#'}, @racketmetafont{#`}, and @racketmetafont{#,}
provide a templating system for syntax objects,@note{Specifically, these
forms implement Lisp's quote, quasiquote, and unquote on syntax objects
instead of lists.} and @racket[syntax-case] performs pattern matching
on @nonbreaking{syntax objects}.

The syntax extension @racket[if-r] is expanded at compile-time, while
the resulting @racket[if] expression is run at runtime.
At compile time, @racket[if-r] looks up the profile information
attached to each branch, using @racket[profile-query].
Using profile information, @racket[if-r] conditionally generates an
@racket[if] expression whose branches are ordered by how likely they are
to be executed.
When the false branch is executed more frequently than the true branch,
@racket[if-r] expands into an @racket[if] expression by negating the test
and swapping the branches.
Otherwise, @racket[if-r] expands into an @racket[if] expression by
keeping the original test and branches.
While this transformation is not meaningful, it strongly resembles the
optimization we present in @Secref{study-case}.
