#lang racket

(require
  "../profiling/exact-interface.rkt")

(provide run)

(define (random-char)
  (integer->char (+ 65 (random (- 122 65)))))

(define (run x)
  (for ([_ (in-range x)])
    (define ls (vector 1 2 3 4))
    (vector-ref ls 0) 
    (vector-ref ls 1)
    (vector-ref ls 2)
    (vector-ref ls 3)
    (vector-ref ls 3)
    (define ls2 (vector 1 2 3 4))
    (vector-ref ls2 0)
    (vector-ref ls2 1)
    (vector-ref ls2 3)))
