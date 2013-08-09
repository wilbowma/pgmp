#lang scribble/base
@section[#:tag "api" "API"]
@(require scribble/manual)
This section presents the API for the profile system used in later examples.

The system requires the following four primitives. We present the
primitives here so the reader can understand the examples in the next
section, but delay discussion of the implementation until @secref{implementation}.
@itemlist[
  @item[@racket[profile-query-weight]]
  @item[@racket[profile-load-data]]
  @item[@racket[profile-dump-data]]
  @item[@racket[compile-profile]]]

@racket[compile-profile] is a parameter used to enable profiling.
@racket[compile-profile] is @racket[#f] by default and can be set to
@racket['source] or @racket['block]. When @racket[compile-profile] is
@racket['source], compiled programs are instrumented to collect source
level profile information that can be used in macros. When
@racket[compile-profile] is @racket['block], compiled programs are
instrumented to collect block-level profile information that can be used
in the compiler back-end.

@racket[profile-dump-data] is used to dump any profile information that
has been collected to a file.

@racket[profile-load-data] is used to load previously dumped data. 

@racket[profile-query-weight] is used to retrieve the weighted profile
counts that have been loaded from a file via @racket[profile-load-data].
@racket[profile-query-weight] takes a source or syntax object. When
writing Scheme macros, we primarily use syntax objects since the macro
system manipulates Scheme syntax objects, but manually constructing
source objects from a source file and expression position can be useful
when manipulating generated Scheme code that should correspond to some
higher-level source code. @racket[profile-query-weight] returns weight
the weighted profile information, or @racket[#f] if there is no
profile information associated with the syntax or source object.
