#lang racket
(require scribble/base
         scriblib/autobib
         scriblib/bibtex
         racket/date)
(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography #:style author+date-style)
(define-bibtex-cite* "bib.bib" ~cite citet ~citea citeta)

(define code-repo
  (make-bib
    #:title "Code repository"
    #:author (authors "???")))

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

(define lattner02
  (make-bib
    #:title "LLVM: An infrastructure for multi-stage optimization"
    #:author "Chris Arthur Lattner"
    #:location (dissertation-location
                 #:institution "University of Illinois"
                 #:degree "Master")
    #:date 2002))

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
                 "of Conference on Programming Language
                 Design and Implementation (PLDI)"
                 #:pages '(132 141))
    #:date 2011
    #:url "http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf"))

(define gcc
  (make-bib
    #:title "Optimize Options - Using the GNU Compiler Collection"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #:url "http://gcc.gnu.org/onlinedocs/gcc-4.7.2/gcc/Optimize-Options.html#index-fprofile_002duse-867"))

(define .net
  (make-bib
    #:title "Profile-Guided Optimizations"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #:url "http://msdn.microsoft.com/en-us/library/e7k32f4k(v=vs.90).aspx"))

(define boost
  (make-bib
    #:title "Boost C++ Libraries"
    #:author (authors "B. Dawes" "D. Abrahams")
    #:date 2009
    #:url "http://www.boost.org"))

(define dybvig09csug
  (make-bib
    #:title "Chez Scheme Version 8 User's Guide"
    #:author "R. Kent Dybvig"
    #:location (book-location #:edition "8.4" #:publisher "Cadence Research Systems")
    #:url "http://www.scheme.com/csug8"
    #:date 2011))

(define csug-ch11 (in-bib dybvig09csug ", Chapter 11"))

#;(define gupta02
  (make-bib
    #:title (elem "Profile Guided Code Optimization. In " (editor (authors "YN Srikant" "Priti Shankar")))
    #:author (authors "R. Gupta" "E. Mehofer" "Y. Zhang")
    #:location (journal-location
                 "The compiler design handbook: optimizations and machine code generation")
    #:date 2002))

(define plt-tr1
  (make-bib #:title    "Reference: Racket"
            #:author   (authors "Matthew Flatt" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-1")
            #:url      "http://racket-lang.org/tr1/"))

(define stamour14
  (make-bib #:title "Feature-specific profiling"
            #:author (authors "Vincent St-Amour" "Mattias Felleisen")
            #:date 2014
            #:location (techrpt-location #:institution "Northeastern
                                         University"
                                         #:number "NU-CCIS-8-28-14-1")
            #:url "http://www.ccs.neu.edu/racket/pubs/NU-CCIS-14-01.pdf"))

(define scala-overview-tech-report
  (make-bib #:title "An Overview of the Scala Programming Language"
            #:author (authors "Martin Odersky" "Philippe Altherr" "Vincent Cremet"
                              "Burak Emir" "Sebastian Maneth" "Stéphane Micheloud" 
                              "Nikolay Mihaylov" "Michel Schinz" "Erik Stenman" 
                              "Matthias Zenger")
            #:date 2004
            #:location (techrpt-location #:institution "EPFL Lausanne"
                                         #:number "IC/2004/64")
            #:url "http://lampwww.epfl.ch/~odersky/papers/ScalaOverview.html"))
