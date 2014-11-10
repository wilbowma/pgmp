#lang scribble/base
@(require
   "defs.rkt"
   "bib.rkt"
   scribble/manual
   scriblib/figure)

@title[#:tag "example"]{A running example}
We first introduce a Scheme macro which we will use as a running example
and to famliarize readers with Scheme's meta-programming system. this
example is not to be taken as a useful optimization.
@figure-here["sample-macro" "Sample macro"
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define-syntax (if-r stx)
  (syntax-case stx ()
    [(if-r test t-exp f-exp)
     (let ([t-prof (profile-query #'t-exp)]
           [f-prof (profile-query #'f-exp)])
       (if (< t-prof f-prof)
           #'(if (not test) f-prof t-prof)
           #'(if test t-prof f-prof)))])))]

In @figure-ref{sample-macro}, we create a new macro with
@racket[define-syntax]. A macro takes a single piece of syntax as its
argument. For example, the function in
@figure-ref{if-r-eg} uses the new macro @racket[if-r]. he
implementation of macro @racket[if-r] will receive the argument
@racket[#'(if-r (subject-contains-ci email "PLDI") (flag email 'important) (flag email 'spam))], which is a data
representation of syntax called a syntax object. The forms @racketmetafont{#'},
@racketmetafont{#`}, and @racketmetafont{#,} implement Lisp's quote,
quasiquote, and unquote but on syntax instead of lists. The form
@racket[syntax-case] perform pattern matching on syntax objects.
@figure-here["if-r-eg" (elem "Using " @racket[if-r])
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define (classify email)
  (if-r (subject-contains-ci email "PLDI")
        (flag email 'important)
        (flag email 'spam))))]

The definition of @racket[if-r] defines a new syntactic form similar to
@racket[if]. At compile time, it looks up the profile information
attached to each expressions, and reorders the branches based on which
is more likely to be executed. This macro is run at compile-time, while
the resulting @racket[if] will be run at run-time.
@Figure-ref{if-r-expand} shows the resulting code after @racket[if-r] is
run.
@figure-here["if-r-expand" (elem "Reslt of " @racket[if-r] " meta-program")
@#reader scribble/comment-reader #:escape-id UNSYNTAX
(RACKETBLOCK0
(define (classify email)
  (if (not (subject-contains-ci email "PLDI"))
      (flag email 'spam)
      (flag email 'important))))]
