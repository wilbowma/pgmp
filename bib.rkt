#lang racket
;(provide ~cite citet generate-bibliography)
;(require scriblib/bibtex
;         scriblib/autobib
;         racket/runtime-path)
;(define-runtime-path bib "bib.bib")
;(define-bibtex-cite bib ~cite citet generate-bibliography #:style number-style)

(require scriblib/autobib)
(require racket/date)
(provide (all-defined-out))
(define-cite ~cite citet generate-bibliography #:style number-style)

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

(define gcc
  (make-bib 
    #:title "Optimize Options - Using the GNU Compiler Collection"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #:url
    "http://gcc.gnu.org/onlinedocs/gcc-4.7.2/gcc/Optimize-Options.html#index-fprofile_002duse-867"))

(define .net
  (make-bib 
    #:title "Profile-Guided Optimizations"
    #:date (seconds->date (find-seconds 0 0 0 20 08 2013))
    #:url "http://msdn.microsoft.com/en-us/library/e7k32f4k(v=vs.90).aspx"))
