#lang racket/base

(provide timing-profile)

(require profile/sampler profile/analyzer profile/render-text
         racket/serialize
         (for-syntax racket/base syntax/parse)
         (for-syntax "timing-interface.rkt"))

;; This is the *exact* same code as OC's `optimization-coach-profile', funny
;; enough. I guess that makes that code LGPL.
(define-syntax (macro-profile stx)
  (syntax-parse stx
    ;; You may want to get rid the `#:use-errortrace?' option, since your thing
    ;; is unlikely to work well without errortrace.
    [(_ (~optional (~seq #:use-errortrace? u-e:expr) #:defaults ([u-e #'#t]))
        body ...)
     (syntax-property
      #`(let ([sampler
               (create-sampler #:use-errortrace? u-e (current-thread) 0.005)])
          body ...
          (sampler 'stop)
          (define samples (sampler 'get-snapshots))
          ;; Remove that if you don't want to print the profile.
          (render (analyze-samples samples))
          ;; TODO add kw arg to control filename
          (with-output-to-file
              ;; May do weird things to unsaved files.
              #,(source-file->profile-file (syntax-source stx))
            #:exists 'replace
            (lambda () (write (serialize samples)))))
      ;; I'm a bad person. Get rid of this if you don't plan on using this from
      ;; any TR files.
      'typechecker:ignore #t)]))
