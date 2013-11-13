#lang scribble/base
@(require "defs.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "conclusion" "Conclusion"]

@section{Comparison to other meta-programming languages}
We take advantage of Scheme's powerful meta-programming facilities that
allows running full-fledged Scheme programs at compile time.  While many
programming langauges have meta-programming systems, their
expressiveness and support for manipulating syntax varies.

@todo{figure out if (profile-query-weight ) could be run in C++,
MetaOCaml at compile time}

@para{C++} limits inspecting syntax and does not allow IO at
compile-time. Without compile-time IO, it seem that template
meta-programming in C++ cannot currently support profile directed
optimization through meta-programming. 

@para{Template Haskell} @todo{cite} supports generating Haskell code via
quotes and splicing, similar to Scheme and MetaOCaml, but also provides
constructors to create Haskell ASTs directly. Template Haskell allows IO
and running ordinary Haskell functions at compile-time. This suggests 
compilers DSLs written in Haskell can easily incorporate a source
expression profiler and allow profile directed optimizations at compile
time. Because Template Haskell can manipulate Haskell syntax, it should
be simple to even write an optimization for Haskell in Template Haskell. 

@para{MetaOCaml} @todo{cite} supports generating OCaml code through
quotes and splicing, similar to Scheme and Template Haskell. MetaOCaml
allows IO and running ordinary OCaml functions at compile-time, however,
it discourages inspecting OCaml syntax. This suggest compilers for DSLs
written as OCaml data can easily incorporate a source expression
profiler and allow  profile directed optimizations at compile time.

@todo{Look at some other systems}
@todo{cite Czarnecki04} for more in-depth comparison of those three.
