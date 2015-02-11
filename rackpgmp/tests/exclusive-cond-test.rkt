#lang racket

(require
  "../pgmp/exclusive-cond.rkt"
  rackunit)
(provide run)

(define flag #f)
(define (random! x) (unless flag (set! flag x)) (random))

(define (run x [profiled? #f])
  (for ([x (in-range x)])
    (exclusive-cond
      [(< (random! 'bad) .3) (sleep 0.00000001)]
      [(< .3 (random! 'bad) .31) (sleep 0.00000001)]
      [(< .31 (random! 'bad) .32) (sleep 0.00000001)]
      [(< .32 (random! 'bad) .33) (sleep 0.00000001)]
      [(< .33 (random! 'bad) .34) (sleep 0.00000001)]
      [(< .34 (random! 'bad) .35) (sleep 0.00000001)]
      [(< .35 (random! 'bad) .36) (sleep 0.00000001)]
      [(< .36 (random! 'bad) .37) (sleep 0.00000001)]
      [(< .37 (random! 'bad) .38) (sleep 0.00000001)]
      [(< .38 (random! 'bad) .39) (sleep 0.00000001)]
      [(>= (random! 'good) .39) (sleep 0.00000001)]))
  (when profiled? (check-equal? flag 'good)))
