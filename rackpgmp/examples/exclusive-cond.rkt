#lang racket

;; First run
;; > raco pgmp --profile example/exclusive-cond.rkt
;;
;; The test suite will fail the first time, since I will not yet be
;; optimized.
;;
;; Next run
;; > racket -t example/exclusive-cond.rkt
;;
;; The test suite will pass, silently.
(module+ main
  (require pgmp/exclusive-cond rackunit)
  (define flag #f)
  (define (set-flag x) (unless flag (set! flag x)))
  (for ([x (list 1 2 3 4 5 #\x #\a 'a 'b 'c)])
    (exclusive-cond
      [(begin (set-flag 'bad) (char? x)) (displayln "A char")]
      [(begin (set-flag 'good) (number? x)) (displayln "A number")]
      [(begin (set-flag 'bad) (symbol? x)) (displayln "A symbol")]))
  (check-equal? flag 'good))
