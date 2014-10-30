#lang racket

(require
  compatibility/mlist
  "../profiling/exact-interface.rkt")

(provide run)

(define (run x)
  (for ([_ (in-range x)])
    (define ls (mlist 1 2 3 4))
    (define ls2 (mcons 1 ls))
    (mmap values ls2)
    (define (mlist-set! ls i v)
      (if (zero? i)
        (set-mcar! ls v)
        (mcons (mcar ls) (mlist-set! (mcdr ls) (sub1 i) v))))
    (mlist-set! ls2 2 2)))
