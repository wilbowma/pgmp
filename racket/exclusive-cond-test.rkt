#lang racket

(require
  rackunit
  "exclusive-cond.rkt"
#;  "errortrace-interface.rkt"
#; (only-in errortrace execute-counts-enabled get-execute-counts))
#;(profiling-enabled #t)
#;(execute-counts-enabled #t)
#;(provide (all-defined-out))

#;(profile-load-data "meow.profile")

(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(define flag #f)
(define (random! x) (unless flag (set! flag x)) (random))
(define (run)
  (for ([x (in-range 10000)])
    (exclusive-cond
      [(<= (random! 'bad) .3) (begin (fact 10) (sleep 0.0001))]
      [(> (random! 'good) .3) (sleep 0.0001)])))

#;(macro-profile (run))
(run)
(check-equal? flag 'good)
#;(profile-dump-data "meow.profile")
#;(displayln (get-profile-results))
