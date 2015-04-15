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
The transformation we present is not a meaningful optimization and is
used only for illustrative purposes.
The structure of this transformation strongly resembles the optimization
we present in @Secref{study-case}.
@figure-here["sample-macro" "Example syntax extension"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (if-r stx)
  (syntax-case stx ()
    [(if-r test t-branch f-branch)
     ; This @racket[let] expression runs at compile time
     (let ([t-prof (profile-query #'t-branch)]
           [f-prof (profile-query #'f-branch)])
       ; This @racket[cond] expression runs at
       ; compile time, and conditionally
       ; generates run-time code based on profile
       ; information.
       (cond
          [(< t-prof f-prof)
          ; This @racket[if] expression would run at
          ; run time when generated.
           #'(if (not test) f-branch t-branch)]
          [(>= t-prof f-prof)
          ; So would this @racket[if] expression.
           #'(if test t-branch f-branch)]))]))

; Example use of @racket[if-r]
(define (classify email)
  (if-r (subject-contains email "PLDI")
        (flag email 'important)
        (flag email 'spam))))]

In @Figure-ref{sample-macro},
@racket[define-syntax] introduces a new syntax extension @racket[if-r].
A syntax extension can be thought of as a function from source
expressions to source expressions.
The compiler rewrites any uses of @racket[if-r] using the code in the
body of @nonbreaking{the extension.}

When used at the bottom of @Figure-ref{sample-macro}, the syntax
extension @racket[if-r] receives the argument:
@racketblock0[
#'(if-r (subject-contains-ci email "PLDI")
        (flag email 'important)
        (flag email 'spam))]
This is a data representation of a term called a syntax object.
The forms @racket[#'code:blank], @racket[#`code:blank], and
@RACKET[#,code:blank] provide a templating system for syntax
objects,@note{Specifically, these forms implement Lisp's quote,
quasiquote, and unquote on syntax objects instead of lists.} and
@racket[syntax-case] performs pattern matching on @nonbreaking{syntax
objects.}

The syntax extension @racket[if-r] expands at compile time, while
the resulting @racket[if] expression runs at run time.
At compile time, the @racket[if-r] expression uses
@racket[profile-query] to look up the profile information attached to
each branch.
Using this profile information, the @racket[if-r] expression
conditionally generates an @racket[if] expression whose branches are
ordered by how likely they are to be executed.
When the false branch is executed more frequently than the true branch,
the @racket[if-r] expression generates an @racket[if] expression by
negating the test and swapping the branches.
Otherwise, the @racket[if-r] expression generates an @racket[if]
expression by keeping the original test and branches.
@Figure-ref{sample-expansion} shows an example of the code the
@racket[if-r] expression from @Figure-ref{sample-macro} could generate.
@figure["sample-expansion" (elem "Example output of " @racket[if-r])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
; Assuming profile information tells us:
;   @racket[(flag email 'important)] runs @racket[5] times
;   @racket[(flag email 'spam)]      runs @racket[10] times
; Then the code after expanding @racket[if-r] is:
(define (classify email)
  (if (not (subject-contains email "PLDI"))
      (flag email 'spam)
      (flag email 'important))))]

