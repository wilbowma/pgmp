#lang racket

(require
  "../perflinty/vector.rkt"
  "../profiling/exact-interface.rkt")

(provide run)

(define (run x)
  (for ([_ (in-range x)])
    (define ls (vector 1 2 3 4))
    (define ls2 (vector-append (vector 1) ls))
    (define ls3 (vector-append (vector 2) ls2))
    (vector-set! ls2 2 2)))
