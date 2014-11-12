#lang scribble/sigplan @preprint @10pt
@(require "bib.rkt")

@title{Profile-Guided Meta-Programming}
@;@(authorinfo "William J. Bowman" "wilbowma@ccs.neu.edu" "Northeastern
@;University")
@;@(authorinfo "Swaha Miller" "swamille@cisco.com" "Cisco Systems, Inc")
@;@(authorinfo "R. Kent Dybvig" "dyb@cisco.com" "Cisco Systems, Inc")
@;@(authorinfo "Vincent St-Amour" "stamourv@ccs.neu.edu" "Northeastern
@;University")
@include-abstract{abstract.scrbl}
@include-section{intro.scrbl}
@include-section{example.scrbl}
@include-section{design.scrbl}
@include-section{implementation.scrbl}
@include-section{case-studies.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}
@(generate-bibliography)
