#lang scribble/sigplan @preprint
@(require "bib.rkt")

@title{Profile directed meta-programming}
@(author (author+email "William J. Bowman" "wilbowma@ccs.neu.edu")
         (author+email "Swaha Miller" "swamille@cisco.com")
         (author+email "R. Kent Dybvig" "dyb@cisco.com") )
@include-abstract["abstract.scrbl"]
@include-section["intro.scrbl"]
@;@include-section["intro-to-macros.scrbl"]
@include-section["design.scrbl"]
@include-section["examples.scrbl"]
@include-section["implementation.scrbl"]
@;@;@include-section["results.scrbl"]
@include-section["related.scrbl"]
@(generate-bibliography)
