#lang racket
(require "exclusive-cond.rkt" (for-syntax rnrs/hashtables-6))

(provide case)

(define-syntax (case x)
  (define (helper key-expr clause* els?)
    (struct clause ((keys #:mutable) body))
    (define (parse-clause c)
      (syntax-case c ()
        [((k ...) e1 e2 ...) (clause #'(k ...) #'(e1 e2 ...))]
        [_ (raise-syntax-error c "invalid case clause")]))
    (define (emit clause*)
      #`(let ([t #,key-expr])
          (exclusive-cond
            #,@(map (lambda (clause)
                      #`[(memv t '#,(clause-keys clause))
                         #,@(clause-body clause)])
                    clause*)
            . #,els?)))
    (let ([clause* (map parse-clause (syntax->list clause*))])
       (define ht (make-hashtable equal-hash equal?))
       (define (trim-keys! clause)
         (set-clause-keys! clause
            (let f ([keys (syntax->list (clause-keys clause))])
              (if (null? keys)
                  '()
                  (let* ([key (car keys)]
                         [datum-key (syntax->datum key)])
                    (if (hashtable-ref ht datum-key #f)
                        (f (cdr keys))
                        (begin
                          (hashtable-set! ht datum-key #t)
                          (cons key (f (cdr keys))))))))))
                (for-each trim-keys! clause*)
                (emit clause*)))
    (syntax-case x (else)
      [(_ e clause ... [else e1 e2 ...])
       (helper #'e #'(clause ...) #'([else e1 e2 ...]))]
      [(_ e clause ...)
       (helper #'e #'(clause ...) #'())]))
