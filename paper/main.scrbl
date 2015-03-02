#lang scribble/sigplan @preprint
@(require scriblib/footnote)
@(require "defs.rkt" "bib.rkt")

@title{Profile-Guided Meta-Programming}
@(authorinfo
  (elem
   "William J. Bowman"
   (titlenote "Author's current affiliation: Northeastern University"))
  (elem "Cisco Systems, Inc and " (linebreak) "Northeastern University ") "wilbowma@ccs.neu.edu")
@(authorinfo
  (elem "Swaha Miller"
   (titlenote "Author's current affiliation: VMware, Inc"))
  "Cisco Systems, Inc"
  "swaham@vmware.com")
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
