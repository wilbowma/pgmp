#lang racket

(require
  "exclusive-cond.rkt"
  "exact-interface.rkt")

(provide run)

(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(define flag #f)
(define (random! x) (unless flag (set! flag x)) (random))
(define (run)
  (for ([x (in-range 10000)])
    (exclusive-cond
      [(<= (random! 'bad) .3) (fact 10) (sleep 0.0001)]
      [(> (random! 'good) .3) (sleep 0.0001)]))
  (equal? flag 'good))
