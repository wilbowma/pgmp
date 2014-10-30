#lang racket

(require
  "../perflinty/auto.rkt"
  "../profiling/exact-interface.rkt")

(provide run)

(define (run x)
  (define ls (seq 1 2 3 4))
  (define ls2 (seq 1 2 3 4))
  (for ([_ (in-range x)])
    (seq-first ls)
    (seq-ref ls 1)
    (seq-ref ls 2)
    (seq-ref ls 3)
    (seq-ref ls 3)
    (seq-first ls2)
    (seq-ref ls2 1)
    (seq-ref ls2 3))
  (displayln (real:list? (test:seq-rep-s ls)))
  (displayln (real:list? (test:seq-rep-s ls2)))
  (displayln (real:vector? (test:seq-rep-s ls)))
  (displayln (real:vector? (test:seq-rep-s ls2))))
