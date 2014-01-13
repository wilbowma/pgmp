#lang racket
(require (for-syntax "macro-interface.rkt"))

(provide exclusive-cond)

(define-syntax (exclusive-cond x)
  (struct clause (syn count))
  ;; load-profile
  ;; Can take a syntax object, in which case it looks up the profile file
  ;; based on its source location info, or a path string or a path pointing
  ;; to the profile file directly.
  ;; Returns a function mapping syntax objects to total time and self time.
  (define look-up (load-profile x))
  (define (make-clause e1 e2)
          (clause e1
            (let-values ([(total self) (look-up e2)])
              (printf "~a: ~a\n" e1 self)
              (or self 0))))
  (define parse-clause
    (lambda (clause)
      (syntax-case clause (=>)
        #;[(e0) (make-clause clause ???)]
        [(e0 => e1) (make-clause clause #'e1)]
        ;; In this case, we could get some bad timing by just measuring
        ;; e1
        [(e0 e1 e2 ...) (make-clause clause #'e1)]
        [_ (raise-syntax-error clause "invalid exclusive-cond clause")])))
  (define (sort-clauses clause*)
    (sort (map parse-clause clause*)
          (lambda (cl1 cl2) (> (clause-count cl1) (clause-count cl2)))))
  (define (reorder-cond clause* els?)
    #`(cond #,@(map clause-syn (sort-clauses (syntax->list clause*))) . #,els?))
    (syntax-case x (else)
    [(_ m1 ... (else e1 e2 ...))
     (reorder-cond #'(m1 ...) #'([else e1 e2 ...]))]
    [(_ m1 ...)
     (reorder-cond #'(m1 ...) #'())]))

