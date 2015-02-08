#lang racket

(require
  "../examples/class.rkt"
  (only-in racket/math pi sqr)
  rackunit)
(provide run)

(class B1 () (define-method (area this) 1))
(class B2 () (define-method (area this) 1))
(class B3 () (define-method (area this) 1))
(class B4 () (define-method (area this) 1))
(class B5 () (define-method (area this) 1))

(class Square
  ((length 0))
  (define-method (area this)
    1 #;(sqr (field this length))))

(class B6 () (define-method (area this) 1))

(class Circle
  ((radius 0))
  (define-method (area this)
    1
    #;(* pi (sqr (field this radius)))))

(class B7 () (define-method (area this) 1))

(define c (new Circle))
(define s (new Square))
(define b1 (new B1))
(define b2 (new B2))
(define b3 (new B3))
(define b4 (new B4))
(define b5 (new B5))
(define b6 (new B6))
(define b7 (new B7))

(field-set! c radius 5)
(field-set! s length 5)

(define (random-in ls)
  (vector-ref ls (random (vector-length ls))))

(define (run runs [profiled? #f])
  (for ([_ (in-range runs)])
    (method (random-in (vector c c c c c s s s b1 b2 b3 b4 b5 b6 b7)) area))
  (when profiled?
    (check-equal?
      (take (inspect-receiver-class-list) 2)
      '(Circle Square)))
  (inspect-receiver-class-list '())
  (for ([_ (in-range runs)])
    (method (random-in (vector c c c s s s s s b1 b2 b3 b4 b5 b6 b7)) area))
  (when profiled?
    (check-equal?
      (take (inspect-receiver-class-list) 2)
      '(Square Circle))))
