#lang scribble/sigplan @preprint 
@(require "bib.rkt")

@title{Profile-guided meta-program optimization}
@(authorinfo "William J. Bowman" "wilbowma@ccs.neu.edu" "Northeastern
University")
@(authorinfo "Swaha Miller" "swamille@cisco.com" "Cisco Systems, Inc")
@(authorinfo "R. Kent Dybvig" "dyb@cisco.com" "Cisco Systems, Inc")
@include-abstract{abstract.scrbl}
@include-section{intro.scrbl}
@include-section{design.scrbl}
@include-section{examples.scrbl}
@include-section{implementation.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}
@(generate-bibliography)
