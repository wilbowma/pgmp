#lang racket
(require scriblib/autobib
         scriblib/bibtex
         racket/date)
(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography #:style number-style)
(define-bibtex-cite* "bib.bib" ~cite citet ~citea citeta)

(define conte96
  (make-bib
    #:title "Accurate and practical profile-driven compilation using the profile buffer"
    #:author (authors "Thomas M Conte" "Kishore N Menezes" "Mary Ann
                      Hirsch")
    #:date 1996
    #:location (proceedings-location 
                 "Annual ACM/IEEE international symposium on Microarchitecture"
                 #:series 29
                 #:pages '(36 45))
    #:url "http://pdf.aminer.org/000/244/348/commercializing_profile_driven_optimization.pdf"))

(define chen10
  (make-bib 
    #:title "Taming Hardware Event Samples for FDO Compilation"
    #:author (authors "Deheo Chen" 
                      "Neil Vachharajani" 
                      "Robert Hundt" 
                      "Shih-wei Liao"
                      "Vinodha Ramasamy"
                      "Paul Yuan" 
                      "Wenguang Chen" 
                      "Weimin Zheng")
    #:date 2010
    #:location (proceedings-location 
                 "Annual IEEE/ACM international symposium on Code
                 generation and optimization"
                 #:series 8
                 #:pages '(42 52))
    #:url "http://hpc.cs.tsinghua.edu.cn/research/cluster/papers_cwg/tamingsample.pdf"))

(define lattner02
  (make-bib
    #:title "LLVM: An infrastructure for multi-stage optimization"
    ;#:author "Chris Authors Lattner"
    #:location (dissertation-location 
                 #:institution "University of Illinois"
                 #:degree "Master")
    #:date 2002))

(define erdweg11
  (make-bib
    #:title "SugarJ: Library-based Syntactic Language Extensibility"
    #:author (authors "Sebastian Erdweg"
                      "Tillmann Rendel"
                      "Christian Kästner"
                      "Klaus Ostermann")
    #:location (proceedings-location 
                 "Proceedings of Conference on Object-Oriented Programming, Systems, Languages, and Applications (OOPSLA)"
                 #:pages '(391 406))
    #:url "http://www.informatik.uni-marburg.de/~seba/publications/sugarj.pdf"
    ;; TODO: autobib chokes when url contains %20 or %7E
    #:date 2011))

(define taha00 
  (make-bib
    #:title "MetaML and multi-stage programming with explicit annotations "
    #:author (authors "Walid Taha" "Time Sheard")
    #:location (journal-location
                 "Theoretical Computer Science"
                 #:pages '(211 242)
                 #:number '(1 2)
                 #:volume 248)
    #:date 2000
    #:url "http://www.cs.rice.edu/~taha/publications/journal/tcs00.pdf"))

(define czarnecki04
  (make-bib
    #:title "DSL implementation in MetaOCaml, Template Haskell, and C++"
    #:author (authors "Krzysztof Czarnecki"
                      "John T O'Donnell"
                      "Jörg Striegntiz"
                      "Walid Taha")
    #:location (proceedings-location 
                 "Domain-Specific Program Generation"
                 #:pages '(51 72)
                 #:volume "Springer Berlin Heidelberg.")
    #:url "http://camlunity.ru/swap/Library/Computer Science/Metaprogramming/Domain-Specific Languages/DSL Implementation in MetaOCaml, Template Haskell and C++.pdf"
    #:date 2004))

(define sheard02
  (make-bib
    #:title "Template meta-programming for Haskell"
    #:author (authors "Time Sheard"
                       "Simon Peyton Jones")
    #:location (proceedings-location
                 "ACM SIGPLAN workshop on Haskell")
    #:date 2002
    #:url "http://research.microsoft.com/en-us/um/people/simonpj/Papers/meta-haskell/meta-haskell.pdf"))

(define dybvig93
  (make-bib
    #:title "Syntactic abstraction in Scheme"
    #:author (authors "R. Kent Dybvig"
                       "Robert Hieb"
                       "Carl Bruggeman")
    #:location (journal-location 
                 "Lisp and symbolic computation"
                 #:pages '(295 326)
                 #:number 4
                 #:volume 5)
    #:date 1993
    #:url "http://pdf.aminer.org/001/006/789/syntactic_abstraction_in_scheme.pdf"))

(define felleisen04 
  (make-bib
    #:title "Building little languages with macros."
    #:author (authors "Matthias Felleisen" "R. Findler" "Matthew Flatt"
                      "Shriram Krishnamurthi")
    #:location (journal-location
                 "Dr. Dobb's Journal"
                 #:pages '(45 49)
                 #:number 4)
    #:date 2004
    #:url "http://fortissimo.unice.fr/twiki/pub/Minfo03/DrK/Building.pdf"))

(define tobin-hochstadt11
  (make-bib
    #:title "Languages as Libraries"
    #:author (authors "Sam Tobin-Hochstadt" "Vincent St-Amour" "Ryan
                       Culpepper" "Matthew Flatt" "Matthias Felleisen")
    #:location (proceedings-location 
                 "Proceedings of Conference on Programming Language
                 Design and Implementation (PLDI)"
                 #:pages '(132 141))
    #:date 2011
    #:url "http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf"))

(define burger98
  (make-bib
    #:title "An infrastructure for profile-driven dynamic recompilation."
    #:author (authors "Robert G. Burder" "R. Kent Dybvig")
    #:location (proceedings-location
                 "Computer Languages, 1998. Proceedings. 1998
                 International Conference on"
                 #:pages '(240 249))
    #:date 1998
    #:url "http://pdf.aminer.org/000/289/483/an_infrastructure_for_profile_driven_dynamic_recompilation.pdf"))
 
(define gcc
  (make-bib 
    #:title "Optimize Options - Using the GNU Compiler Collection"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #;#:url #;"http://gcc.gnu.org/onlinedocs/gcc-4.7.2/gcc/Optimize-Options.html#index-fprofile_002duse-867"))

(define .net
  (make-bib 
    #:title "Profile-Guided Optimizations"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #:url "http://msdn.microsoft.com/en-us/library/e7k32f4k(v=vs.90).aspx"))

