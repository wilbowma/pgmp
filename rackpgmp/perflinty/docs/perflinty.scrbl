#lang scribble/manual

@title{Perflinty}
@(require
   (for-label
     (prefix-in builtin: racket/base)
     "../auto.rkt"
     "../vector.rkt"
     "../list.rkt"))

@declare-exporting[#:use-sources (racket/base)]
This guide describes a profile-guided data structure specialization
library similar in spirit to
@hyperlink["https://dl.acm.org/citation.cfm?id=1545076"]{Perflint},
provided by the @racket[rackpgmp] pacakge.
The library currently supports a limited subset of
@racketlink[builtin:list @racketfont{list}] and
@racketlink[builtin:vector @racketfont{vector}] operations.

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
specializes each instance to a @racket[list] or
@racket[vector],
depending on which operations are performed on that instance.
