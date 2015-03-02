#lang racket
(require (for-syntax "api/exact.rkt"))

(provide exclusive-cond)

(define-syntax (exclusive-cond x)
  (struct clause (syn weight))
  (define make-profile-point (make-profile-point-factory "exclusive-cond-branch"))
  (define profile-query-weight (load-profile-query-weight x))
  (define (make-clause e1 e2)
    (clause e1 (or (profile-query-weight e2) 0)))
  (define (parse-clause clause)
    (syntax-case clause (=>)
      #;[(e0) (make-clause clause ???)]
      ;; When exclusive-cond is generated from another macro, the
      ;; branches may not have unique profile points. Best to generate
      ;; them.
      [(e0 => e1)
       (let ([branch-point (make-profile-point #'e1)])
         (make-clause #`(e0 => #,(annotate-syn branch-point e1))
           branch-point))]
      [(test e1 e2 ...)
       (let ([branch-point (make-profile-point #'e1)])
         (make-clause #`(test #,(annotate-syn branch-point e1) e2 ...)
           branch-point))]
      [_ (raise-syntax-error clause "invalid exclusive-cond clause")]))
  (define (sort-clauses clause*)
    (sort (map parse-clause clause*) > #:key clause-weight))
  (define (reorder-cond clause* els?)
    #`(cond #,@(map clause-syn (sort-clauses (syntax->list clause*))) . #,els?))
  (syntax-case x (else)
    [(_ m1 ... (else e1 e2 ...))
     (reorder-cond #'(m1 ...) #'([else e1 e2 ...]))]
    [(_ m1 ...)
     (reorder-cond #'(m1 ...) #'())]))
