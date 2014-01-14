#lang racket

;; Run like that: racket -l errortrace -t example.rkt

(require "profile.rkt"
         (for-syntax syntax/parse "timing-interface.rkt"))

(define-syntax (verbose-if stx)
  (syntax-parse stx
    [(_ test then else)
     (define look-up (load-profile stx))
     (define-values (test-total test-self) (look-up #'test))
     (define-values (then-total then-self) (look-up #'then))
     (define-values (else-total else-self) (look-up #'else))
     (printf "last time:\n")
     (printf "test took ~a ms total (~a ms self)\n" test-total test-self)
     (printf "then took ~a ms total (~a ms self)\n" then-total then-self)
     (printf "else took ~a ms total (~a ms self)\n" else-total else-self)
     #'(if test then else)]))

(define (run)
  (for ([i (in-range 1000)])
    (verbose-if (> (random) 0.5)
                (sleep 0.001)
                (sleep 0.001))))

(macro-profile (run))
