#lang racket

(require "../exclusive-cond.rkt")

(provide run)

(define (run x)
  (define flag #f)
  (define (random! x) (unless flag (set! flag x)) (random))
  (for ([x (in-range x)])
    (exclusive-cond
      [(< (random! 'bad) .3) (sleep 0.000001)]
      [(< .3 (random! 'bad) .31) (sleep 0.000001)]
      [(< .31 (random! 'bad) .32) (sleep 0.000001)]
      [(< .32 (random! 'bad) .33) (sleep 0.000001)]
      [(< .33 (random! 'bad) .34) (sleep 0.000001)]
      [(< .34 (random! 'bad) .35) (sleep 0.000001)]
      [(< .35 (random! 'bad) .36) (sleep 0.000001)]
      [(< .36 (random! 'bad) .37) (sleep 0.000001)]
      [(< .37 (random! 'bad) .38) (sleep 0.000001)]
      [(< .38 (random! 'bad) .39) (sleep 0.000001)]
      [(>= (random! 'good) .39) (sleep 0.000001)]))
  (equal? flag 'good))
