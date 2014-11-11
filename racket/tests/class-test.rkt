#lang racket/base

(require 
  "../class.rkt"
  (only-in racket/math pi sqr)
  rackunit)

(provide run)

(define (run x)
  (define flag #f)
  (class Square
    ((length 0))
    (define-method (area this)
      (sqr (field this length))))
  (class Circle
    ((radius 0))
    (define-method (area this)
      (* pi (sqr (field this radius)))))

  (define c (new Circle))
  (define s (new Square))

  (field-set! c radius 5)
  (field-set! s length 5)

  (define (random-in ls)
    (list-ref ls (random (length ls))))

  (for ([n (in-range x)])
    (method (random-in (list c c c c c s)) area))
  #t)
