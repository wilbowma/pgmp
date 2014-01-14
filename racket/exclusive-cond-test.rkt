#lang racket

(require
  rackunit
  "exclusive-cond.rkt"
  "exact-interface.rkt"
 #;(only-in errortrace execute-counts-enabled get-execute-counts))

(provide run)
#;(execute-counts-enabled #t)

(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(define flag #f)
(define (random! x) (unless flag (set! flag x)) (random))
(define (run)
  (for ([x (in-range 10000)])
    (exclusive-cond
      [(<= (random! 'bad) .3) (fact 10) (sleep 0.0001)]
      [(> (random! 'good) .3) (sleep 0.0001)]))
  (check-equal? flag 'good))

#;(macro-profile (run))
