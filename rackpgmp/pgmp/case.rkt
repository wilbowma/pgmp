#lang racket
(require
  (for-syntax "api/exact.rkt")
  "exclusive-cond.rkt"
  ;; TODO: Using rnrs hashtables to stay close to Scheme implementation, but
  ;; that's not really necessary. Move to Racket hashes
  (for-syntax rnrs/hashtables-6))

(provide case)

(define-syntax (case x)
  (define (helper key-expr clause* els?)
    (struct clause ((keys #:mutable) body inspect))
    (define (parse-clause c)
      ;; inspect allows for testing that branches get reordered correctly
      (syntax-case c (inspect)
        [((k ...) (inspect e) e1 e2 ...)
         (clause #'(k ...) #'(e1 e2 ...) #'e)]
        [((k ...) e1 e2 ...) (clause #'(k ...) #'(e1 e2 ...) #f)]
        [_ (raise-syntax-error c "invalid case clause")]))
    (define (emit clause*)
      #`(let ([t #,key-expr])
          ;; TODO: When exclusive-cond is generated, profile information doesn't
          ;; get loaded properly just loading from the top-level syntax due to
          ;; differences in source location, so manually attached
          ;; source-location.
          ;; TODO: This might indicate loading profile data should be done at a
          ;; global basis, rather than per macro .... but maybe not.
          #,(annotate-syn x
              (exclusive-cond
                  #,@(map (lambda (clause)
                            #`[(begin #,@(if (clause-inspect clause)
                                             #`(#,(clause-inspect clause))
                                             #`())
                               (memv t '#,(clause-keys clause)))
                               #,@(clause-body clause)])
                          clause*)
                  . #,els?))))
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
