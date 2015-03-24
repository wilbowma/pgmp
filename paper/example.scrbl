#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/figure
   scriblib/footnote)

@title[#:tag "example"]{A Running Example}
We first introduce a Scheme syntax extension which we use as a running
example and to familiarize readers with Scheme and Racket style
meta-programming.
@figure-here["sample-macro" "Example syntax extension"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (if-r stx)
  (syntax-case stx ()
    [(if-r test t-branch f-branch)
     ; Rewrites to
     (let ([t-prof (profile-query #'t-branch)]
           [f-prof (profile-query #'f-branch)])
       (if (< t-prof f-prof)
           ; if false branch is more frequent:
           #'(if (not test) f-branch t-branch)
           ; if true branch is more frequent:
           #'(if test t-branch f-branch)))])))]

In @Figure-ref{sample-macro},
@racket[define-syntax] introduces a new syntax extension @racket[if-r].
Any uses of @racket[if-r] in the source will be rewritten using the code
in the body of the extension. The syntax extension can be thought of as
a function from source expressions to @nonbreaking{source expression.}
@figure-here["if-r-eg" (elem "Using " @racket[if-r])
@#reader scribble/comment-reader
(racketblock0
;; Source code
(define (classify email)
  (if-r (subject-contains email "PLDI")
        (flag email 'important) ; Run 5 times
        (flag email 'spam)))    ; Run 10 times

;; Code generated from expanding syntax extension
(define (classify email)
  (if (not (subject-contains email "PLDI"))
      (flag email 'spam)
      (flag email 'important))))]
For example, in @Figure-ref{if-r-eg} the syntax extension @racket[if-r]
receives @nonbreaking{the argument:}
@racketblock0[
#'(if-r (subject-contains-ci email "PLDI")
        (flag email 'important)
        (flag email 'spam))]

This is a data representation of a term called a syntax object. The
forms @racketmetafont{#'}, @racketmetafont{#`}, and @racketmetafont{#,}
provide a templating system for syntax objects@note{Specifically, these
forms implement Lisp's quote, quasiquote, and unquote on syntax objects
instead of lists.}, and @racket[syntax-case] performs pattern matching
on @nonbreaking{syntax objects.}

At compile time, @racket[if-r] looks up the profile information
attached to each branch, using @racket[profile-query], and generates an
@racket[if] expression with the branches of the @racket[if] ordered by
which branch is more likely to be executed.
This transformation is not a meaninful optimizaion and is only used for
illustrative purposes.
The syntax extension is expanded at compile-time, while the resulting
@racket[if] is run at runtime.
