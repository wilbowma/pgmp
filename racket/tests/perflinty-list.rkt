#lang racket

(require
  "../perflinty/list.rkt"
  "../profiling/exact-interface.rkt")

(provide run)

(define (run x)
  (for ([_ (in-range x)])
    (define ls (list 1 2 3 4))
    (car ls)
    (list-ref ls 1)
    (list-ref ls 2)
    (list-ref ls 3)
    (list-ref ls 3)
    (define ls2 (list 1 2 3 4))
    (car ls2)
    (list-ref ls2 1)
    (list-ref ls2 3)))
