#lang racket

(define (describe obj) (printf "Hello ~a\n" obj))

(define mrmcmeow%
  (class object%
    (define/public (describe-self)
      (describe this))
    (super-new)))

(define mrmcmeow2%
  (class object%
    (define/public (describe-self)
      (format "~a hehe\n" (describe this)))
    (super-new)))

(define mrmcmeow3%
  (class object%
    (define/public (describe-self)
      (format "~a hehehe\n" (describe this)))
    (super-new)))

(define mrmcmeow4%
  (class object%
    (define/public (describe-self)
      (format "~a hehehehe\n" (describe this)))
    (super-new)))

(module+ test
(time (parameterize ([current-output-port (open-output-nowhere)])
  (for ([i (in-range 500)])
  (for ([i (in-range 1000)])
      (send (new mrmcmeow2%) describe-self))
  (for ([i (in-range 10000)])
       (send (new mrmcmeow%) describe-self))
  (for ([i (in-range 1520)])
       (send (new mrmcmeow3%) describe-self))))))

