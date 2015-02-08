#lang scribble/manual

@title{Perflinty}
@(require
   (for-label
     "../perflinty/auto.rkt"
     "../perflinty/vector.rkt"
     "../perflinty/list.rkt"))

@defmodule[pgmp/perflinty]

This sections describes a profile-guided data structure specialization
library provided by @racket[pgmp], similar in spirit to
@hyperlink{https://dl.acm.org/citation.cfm?id=1545076}["Perflint"].
The library currently supports a limited subset of @racket[list] and
@racket[vector] operations.

@section{Perflinty Lists}
@defmodule[pgmp/perflinty/list]

This section describes a list library that warns the user when
operations that are asymptotically fast on vectors are being called on
instances of a list more often than operations that are asymptotically
fast on lists.

@section{Perflinty Vectors}
@defmodule[pgmp/perflinty/vector]

This section describes a vector library that warns the user when
operations that are asymptotically fast on lists are being called on
instances of a vector more often than operations that are asymptotically
fast on vectors.

@section{Perflinty Auto}
@defmodule[pgmp/perflinty/auto]

This section describes a sequence library that automatically
specializes each instance to a @racket[list] or @racket[vector],
depending on which operations are performed on that instance.
