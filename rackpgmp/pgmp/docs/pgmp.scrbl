#lang scribble/manual
@(require
  scribble/eval
  racket/sandbox
  (for-label
    (rename-in racket [case builtin:case])
    racket/contract
    syntax/srcloc
    "../main.rkt"))

@(define (gtech . x)
    (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))

@title{PGMP: Profile-Guided Meta-Programming}
This collection provides a similar API to that described in
@hyperlink["https://williamjbowman.com/papers.html#pgmp"]{Profile-Guided
Meta-Programming}. It also provides some useful profile-guided
meta-programs.

@racketmodname[pgmp] uses @racketmodname[errortrace] to collect exact
execution counts of every expression during a sample execution of a program.
During subsequent executions, meta-programs (i.e. @gtech{macro}s)
can load the collected profile data through the API provided by
@racketmodname[pgmp/api/exact], and inform compile-time decisions based
on that profile information.

@margin-note{The @racketmodname[pgmp] module reexports @racketmodname[pgmp/api/exact]
at phase level 0 and 1, and @racketmodname[pgmp/case], and
@racketmodname[pgmp/exclusive-cond] at phase level 0.}

@table-of-contents[]

@section{Examples and Tutorial}

@subsection{My First Profile-Guided Meta-Program}
In this tutorial, we present the API defined in @racketmodname[pgmp/api/exact]
via an example profiled-guided @gtech{macro}. We define @racket[if-r], a
@gtech{macro} that reorders its branches based on which branch is
executed most frequently.

@(define evaler
  (parameterize ([sandbox-path-permissions '((write "program.profile") (write "eval.profile"))])
    (make-base-eval #:lang 'racket/base
      '(require pgmp (for-syntax racket/base))
      '(define _syntax #'_syntax))))
@with-eval-preserve-source-locations[
@interaction[
#:eval evaler
(eval:alts (require pgmp) (void))
(define-syntax (if-r stx)
  (define profile-query
    (let ([f (load-profile-query-weight stx)])
      (lambda (x) (or (f x) 0))))
  (syntax-case stx ()
    [(_ test t f)
     (let ([t-prof (profile-query #'t)]
           [f-prof (profile-query #'f)])
       (if (< t-prof f-prof)
           #'(if (not test) f t)
           #'(if test       t f)))]))]]

First, the function @racket[load-profile-query-weight], which executes at
compile-time, loads any profile information associated with
@racket[(syntax-source stx)] that has been saved from previous
executions and returns a query function. The query function may return
@racket[#f], but in this example we ignore the distinction between
@racket[#f] and @racket[0]  by always returning @racket[0] when the
query returns @racket[#f].

Next, we use @racket[profile-query] to get the @tech{profile weight}
associated with each branch. @racket[profile-query] can accept either a
@racket[syntax?] or @racket[profile-point?]. Here, we query the syntax
of each branch.

Finally, we generate code. If the false branch @racket[f] is executed
most often, then we negate the @racket[test] and flip the branches
when generating the @racket[if] form.
Otherwise, we generate a normal @racket[if] form.

@with-eval-preserve-source-locations[
@interaction[
#:eval evaler
(syntax->datum
  (expand-once
    #'(if-r (subject-contains-ci email "PLDI")
            (flag email 'important)
            (flag email 'spam))))]]

We see that when we expand @racket[if-r] before any profile information is generated
and saved, the test and branches stay in the original order.

Before @racket[if-r] will reorder its branches, we need to generate some
profile information. While we could use @racket[run-with-profiling] and
@racket[save-profile] to actually profile the profile and save the
profiling data, for this example we instead use the API to generate some
new profile points example profile data.

@with-eval-preserve-source-locations[
@interaction[
#:eval evaler
(define make-profile-point (make-profile-point-factory "my-first-pgmp"))
(define profile-point-t (make-profile-point _syntax))
(define profile-point-f (make-profile-point _syntax))]]

We use @racket[make-profile-point-factory] to create a function
that generates fresh profile points using the prefix @racket["my-frsit-pgmp"],
and define two new profile points @racket[profile-point-t] and
@racket[profile-point-f]. The profile point generater expects a
piece of syntax. For this example, we assume we have some appropriate
piece of syntax @racket[_syntax] which we use where needed
places. In practice, this piece of syntax will be something in scope
when defining a new profile-guided @gtech{macro}.

Next, we generate some profile information and save it to
the file expected by @racket[load-profile-query-weight]. We use
@racket[profile-file] to generate the filename based on
@racket[_syntax]. The generated profile information claims that the
expression associated with @racket[profile-point-t] was executed
@racket[10] times, while the expression associated with
@racket[profile-point-f] was executed @racket[20] times. Therefore,
@racket[if-r] should reorder the branches.

@interaction[
#:eval evaler
(require racket/serialize)
(with-output-to-file (profile-file _syntax)
  (lambda ()
    (write (serialize (list (cons profile-point-t 10)
                            (cons profile-point-f 20)))))
  #:exists 'replace)]

Finally, we annotate branches with the generated profile points, so
@racket[profile-query] will find the profile information for the
generated profile points. We also annotate the whole @racket[(if-r _...)]
with @racket[_syntax] so @racket[load-profile-query-weight] will load
the right file.

@with-eval-preserve-source-locations[
@interaction[
#:eval evaler
#:escape UNSYNTAX
(syntax->datum
  (expand-once
   (quasisyntax/loc
     _syntax
     (if-r (subject-contains-ci email "PLDI")
           #,(annotate-syn
             profile-point-t
             (flag email 'important))
           #,(annotate-syn
             profile-point-f
             (flag email 'spam))))))]]

@subsection{Instrumenting and optimizing}

This section presents the standard workflow for profiling and optimizing
a profile-guided @gtech{macro}. We present a small example program using
@racket[case], a profile-guided @gtech{macro} defined by
@racketmodname[pgmp/case].

@racketmod[
#:file "example-parser.rkt"
racket/base
(require pgmp)

_...

(define (parse stream)
 (case (peek-char stream)
  [(#\space #\tab) (white-space stream)]
  [(0 1 2 3 4 5 6 7 8 9) (digit stream)]
  [(#\() (start-paren stream)]
  [(#\)) (end-paren stream)]
  _...))

(module+ main (parse _input-stream))]

Here we define a @racket[module] that uses @racket[case]. To
get the benefits of @racket[case], we first need to instrument
and run the @racket[module]. There are two ways to do this.

@racketmodname[pgmp] provides a commandline utility through
@exec{raco}. We could instrument and profile the module by running
the command
@exec{raco pgmp --profile example-parser.rkt}. To optimize
and run the module, we can henceforth use @exec{racket -t
example-parser.rkt}. @racket[case] will automatically
load and use the profile data from the instrumented run.

Alternatively, @racketmodname[pgmp] also provides API functions to
instrument and profile one @racket[module] from another. We can use the
API to instrument and profile the module,
then optimize and run the module via @racket[dynamic-require].

@interaction[
(eval:alts (run-with-profiling `(submod "example-parser.rkt" main)) (void))
(eval:alts (save-profile "example-parser.rkt") (void))
(eval:alts (dynamic-require `(submod "example-parser.rkt" main) 0) (void))]

@section{API}

@defmodule[#:multi (pgmp pgmp/api/exact) #:no-declare]
@declare-exporting[pgmp/api/exact]

This section describes the API provided by @racketmodname[pgmp] for
meta-programmers to write their own profile-guided meta-programs.

@defproc[(make-profile-point-factory [prefix string?])
         (-> source-location? profile-point?)]{
Returns a function that, given a @racket[source-location?]
@racket[_syn] (such as a @racket[syntax?] or @racket[srcloc?]), generates
a fresh profile point. The generated profile point will be based on
@racket[prefix] and @racket[_syn] to provide useful error messages.
}

@defproc[(profile-point? [expr any/c])
          boolean?]{
Return @racket[#t] if @racket[expr] is a profile points, and @racket[#f]
otherwise.
}

@defform[(annotate-syn profile-point template)
         #:contracts ([profile-point profile-point?])]{

Like @racket[quasisyntax], but attaches @racket[profile-point] to the
syntax objects resulting from @racket[template].
}

@defproc[(profile-file [file-source (or/c source-location? path?  path-string?)])
          path?]{
Generates a @racket[path?] to the file represented by
@racket[file-source], and appends @racket[".profile"] to it.
}

@defproc[(save-profile [file-source (or/c source-location? path? path-string?)])
          void?]{
Saves the current profile execution counts to @racket[(profile-file
file-source)].
}

@defproc[(run-with-profiling [module module-path?]) void?]{
Instruments @racket[module] to collect profiling information and runs
it.
}

@defproc[(load-profile [file-source (or/c source-location? path? path-string?)])
         (values
           (-> (or/c syntax? profile-point?) (or/c natural-number/c #f))
           (-> (or/c profile-point?) (or/c (real-in 0 1) #f)))]{
Loads the profile information @racket[(profile-file file-source)] and
returns two functions that can query that profile information.

The first function returns the exact execution count
associated with a profile point, or @racket[#f] if no profile
information exists for that profile point.

The second function returns the @tech{profile weight} associated with
that profile point, or @racket[#f] is no profile information exists for
that profile point. A @deftech{profile weight} is the ratio of the exact
execution count to the maximum execution count of any other profile
point.
}

@defproc[(load-profile-look-up [file-source (or/c source-location? path? path-string?)])
         (-> (or/c syntax? profile-point?) (or/c natural-number/c #f))]{
Returns the first value returned by @racket[load-profile].
}

@defproc[(load-profile-query-weight [file-source (or/c source-location? path? path-string?)])
         (-> (or/c syntax? profile-point?) (or/c natural-number/c #f))]{
Returns the second value returned by @racket[load-profile].
}

@section{Profile-Guided Conditionals}

@defmodule[pgmp/case #:no-declare]
@declare-exporting[pgmp/case #:use-sources (pgmp/exclusive-cond)]

@defform[(case val-expr case-clause ...)]{
Like Racket's @racketlink[builtin:case @racketfont{case}], but may sort
@racket[case-clause]s in order of most frequently executed. An @racket[else]
clause, if one exists, will always be last.
}

@defmodule[pgmp/exclusive-cond #:no-declare]

@defform[(exclusive-cond exclusive-cond-clause ...)
         #:grammar
         [(exclusive-cond-clause (code:line [test-expr then-body ...+])
                                 (code:line [else then-body ...+])
                                 [test-expr => proc-expr])]]{
Like Racket's @racket[cond], but may sort
@racket[exclusive-cond-clause]s in order of most frequently executed.
An @racket[else] clause, if one exists, will always be last.
Note that the clauses must be mutually exclusive or which branch is
taken is nondeterministic.
}
