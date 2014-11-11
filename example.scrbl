#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/figure)

@title[#:tag "example"]{A running example}
We first introduce a Scheme syntax extension which we use as a running
example and to familiarize readers with Scheme and Racket flavor
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

In @figure-ref{sample-macro},
@racket[define-syntax] introduces a new syntax extension @racket[if-r].
Any uses of @racket[if-r] in the source will be rewritten using the code
in the body of the extension.  For example, the function in
@figure-ref{if-r-eg} uses @racket[if-r]. The
extension will receive the argument:
@racketblock0[
#'(if-r (subject-contains-ci email "PLDI")
        (flag email 'important)
        (flag email 'spam))]

This is a data representation of a term called a syntax object. The
forms @racketmetafont{#'}, @racketmetafont{#`}, and @racketmetafont{#,}
implement Lisp's quote, quasiquote, and unquote but on syntax instead of
lists. @racket[syntax-case] performs pattern matching on syntax
objects.
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

At compile time, @racket[if-r] it looks up the profile information
attached to each branch, reorders the branches based on which
is more likely to be executed. It uses @racket[profile-query] to access
the profile information for each branch and generate an @racket[if]
expression. This transformation is not a meaninful
optimizaion and is only used for illustrative purposes. The syntax
extension is expanded at compile-time, while the resulting @racket[if]
will be run at run-time.
