#lang racket

(require
  rackunit
  "../pgmp/case.rkt")
(provide run)

(define (random-char)
  (integer->char (+ 65 (random (- 122 65)))))

(define flag #f)

(define (run x [profiled? #f])
  (for ([x (in-range x)])
    (let ([c (random-char)])
         (case c
           [(#\A) (sleep 0.000001)]
           [(#\B) (sleep 0.000001)]
           [(#\C) (sleep 0.000001)]
           [(#\D) (sleep 0.000001)]
           [(#\E) (sleep 0.000001)]
           [(#\F) (sleep 0.000001)]
           [(#\G) (sleep 0.000001)]
           [(#\H) (sleep 0.000001)]
           [(#\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
             #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i
             #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
             #\y #\z)
            (inspect (set! flag #t))
            (sleep 0.000001)]
           [else (error 'run "Character I wasn't expecting: ~a" c)])))
  (when profiled? (check-true flag)))
