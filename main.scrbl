#lang scribble/sigplan @preprint @10pt
@(require scriblib/footnote)
@(require "defs.rkt" "bib.rkt")

@title{Profile-Guided Meta-Programming}
@(authorinfo "William J. Bowman"  "Northeastern University" "wilbowma@ccs.neu.edu")
@(authorinfo
  (elem "Swaha Miller" 
   (titlenote "Author's current affiliation: VMware, Inc (swaham@vmware.com)")
  )
  "Cisco Systems, Inc"
  "")
@(authorinfo "Vincent St-Amour"  "Northeastern University" "stamourv@ccs.neu.edu")
@(authorinfo "R. Kent Dybvig"  "Cisco Systems, Inc" "dyb@cisco.com")
@include-abstract{abstract.scrbl}
@include-section{intro.scrbl}
@include-section{example.scrbl}
@include-section{design.scrbl}
@include-section{implementation.scrbl}
@include-section{case-studies.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}
@(generate-bibliography)
