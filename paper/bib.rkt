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

(define lattner02
  (make-bib
    #:title "LLVM: An Infrastructure for Multi-Stage Optimization"
    #:author "Chris Arthur Lattner"
    #:location (dissertation-location
                 #:institution "University of Illinois"
                 #:degree "Master")
    #:date 2002))

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
    #:location (book-location
                 #:edition "8.4"
                 #:publisher "Cadence Research Systems")
    #:url "http://www.scheme.com/csug8"
    #:date 2011))

(define csug-ch11 (in-bib dybvig09csug ", Chapter 11"))

(define plt-tr1
  (make-bib #:title    "Reference: Racket"
            #:author   (authors "Matthew Flatt" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-1")
            #:url      "http://racket-lang.org/tr1/"))

(define scala-overview-tech-report
  (make-bib #:title "An Overview of the Scala Programming Language"
            #:author (authors "Martin Odersky" "Philippe Altherr"
                              "Vincent Cremet" "Burak Emir"
                              "Sebastian Maneth" "Stéphane Micheloud"
                              "Nikolay Mihaylov" "Michel Schinz"
                              "Erik Stenman" "Matthias Zenger")
            #:date 2004
            #:location (techrpt-location #:institution "EPFL Lausanne"
                                         #:number "IC/2004/64")
            #:url "http://infoscience.epfl.ch/record/52656?ln=en"))
