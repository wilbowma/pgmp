#lang scribble/manual
@(require
  (for-label
    (rename-in racket [case builtin:case])
    racket/contract
    syntax/srcloc
    "../main.rkt"))

@title{PGMP -- Profile-Guided Meta-Programming}

@table-of-contents[]

@section{API}

@defmodule[pgmp]

This section describes the API provided by @racket[pgmp] for
meta-programmers to write their own profile-guided meta-programs.

@defproc[(make-profile-point-factory [prefix string?])
         (-> source-location? profile-point?)]{
Returns a function that, given a @racket[source-location?]
@racket[_syn] (such as a @racket[syntax?] or @racket[srcloc?]), generates
a fresh profile point. The generated profile point will be based on
@racket[prefix] and @racket[_syn] to provide useful error messages.
}

@defform[(annotate-syn profile-point template)
         #:contracts ([profile-point profile-point?])]{

Like @racket[quasisyntax], but attaches @racket[profile-point] to the
syntax objects resulting from @racket[template].
}

@defproc[(save-profile [file-source (or/c source-location? path? path-string?)])
          void?]{
Saved the current profile execution counts to
@racket[_filename.profile], where @racket[_filename] is extracted from
@racket[file-source].
}

@defproc[(run-with-profiling [module module-path?]) void?]{
Instruments @racket[module] to collect profiling information and runs
it.
}

@defproc[(load-profile [file-source (or/c source-location? path? path-string?)])
         (values
           (-> profile-point? (or/c natural-number/c #f))
           (-> profile-point? (or/c (real-in 0 1) #f)))]{
Loads the profile information from the file associated with
@racket[file-source] and returns two functions that can query that
profile information. The first function returns the exact execution count
associated with a profile point, or @racket[#f] if no profile
information exists for that profile point. The second function returns
the @emph{profile weight} associated with that profile point. A @emph{profile
weight} is the ratio of the exact execution count to the maximum
execution count of any other profile point.
}

@defproc[(load-profile-look-up [file-source (or/c source-location? path? path-string?)])
         (-> profile-point? (or/c natural-number/c #f))]{
Returns the first value returned by @racket[load-profile].
}

@defproc[(load-profile-query-weight [file-source (or/c source-location? path? path-string?)])
         (-> profile-point? (or/c natural-number/c #f))]{
Returns the second value returned by @racket[load-profile].
}

@section{Profile-Guided Conditionals}

@defmodule[pgmp]

This sections describes several profile-guided conditional braching
forms provided by @racket[pgmp].

@defform[(case val-expr case-clause ...)]{
Like Racket's builtin @racket[builtin:case], but may sort
@racket[case-clause]s in order of most frequently executed. An @racket[else]
clause, if one exists, will always be last.
}

@defform[(exclusive-cond exclusive-cond-clause ...)
         #:grammar
         [(exclusive-cond-clause (code:line [test-expr then-body ...+])
                                 (code:line [else then-body ...+])
                                 [test-expr => proc-expr])]]{
Like Racket's builtin @racket[cond], but may sort
@racket[exclusive-cond-clause]s in order of most frequently executed.
An @racket[else] clause, if one exists, will always be last.
Note that the clauses must be mutually exclusive or which branch is
taken is nondeterministic.
}

@include-section{perflinty.scrbl}
