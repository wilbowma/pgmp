#lang racket

(require
  "../perflinty/auto.rkt"
  rackunit)
(provide run)

(define ls (seq 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(define ls2 (seq 1 2 3 4))

(define (run x [profiled? #f])
  (for ([_ (in-range x)])
    (seq-first ls)
    (seq-ref ls 1)
    (seq-ref ls 2)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-ref ls 13)
    (seq-length ls)
    (seq-length ls)
    (seq-length ls)
    (seq-length ls)
    (seq-length ls)
    (seq-first ls2)
    (seq-first ls2)
    (seq-ref ls2 3))
  (unless profiled? (check-true (list? (test:seq-rep-s ls))))
  (when profiled? (check-true (vector? (test:seq-rep-s ls))))

  (check-true (list? (test:seq-rep-s ls2))))
