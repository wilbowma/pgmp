#lang scribble/sigplan
@(require scriblib/footnote)
@(require "defs.rkt" "bib.rkt")

@title{Profile-Guided Meta-Programming}
@(authorinfo
  (elem
   "William J. Bowman"
   (titlenote "Author's current affiliation: Northeastern University"))
  (elem "Northeastern University and " (linebreak) "Cisco Systems, Inc")
  "wjb@williamjbowman.com")
@(authorinfo
  (elem "Swaha Miller"
   (titlenote "Author's current affiliation: VMware, Inc"))
  "Cisco Systems, Inc"
  "swaham@vmware.com")
@(authorinfo "Vincent St-Amour"  "Northeastern University" "stamourv@ccs.neu.edu")
@(authorinfo "R. Kent Dybvig"  "Cisco Systems, Inc" "dyb@cisco.com")

@(conferenceinfo "PLDI '15" "June 13--17, 2015, Portland, OR, USA")
@(copyrightyear "2015")
@(copyrightdata "978-1-4503-3468-6/15/06")
@(doi "10.1145/2737924.2737990")
@include-abstract{abstract.scrbl}
@include-section{intro.scrbl}
@include-section{example.scrbl}
@include-section{design.scrbl}
@include-section{implementation.scrbl}
@include-section{beyondscheme.scrbl}
@include-section{case-studies.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}
@(generate-bibliography)
