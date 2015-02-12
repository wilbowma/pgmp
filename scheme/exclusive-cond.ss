;; Copyright © 2015 Cisco Systems, Inc

(library (exclusive-cond)
  (export exclusive-cond)
  (import (chezscheme) (utils))

  (define-syntax (exclusive-cond x)
    (define-record-type clause
      (nongenerative)
      (fields (immutable syn) (immutable count))
      (protocol
        (lambda (new)
          (lambda (e1 e2)
            (new e1 (or (profile-query-weight e2) 0))))))
    (define parse-clause
      (lambda (clause)
        (syntax-case clause (=>)
          #;[(e0) (make-clause clause ???)]
          [(e0 => e1) (make-clause clause #'e1)]
          [(e0 e1 e2 ...) (make-clause clause #'e1)]
          [_ (syntax-error clause "invalid exclusive-cond clause")])))
    (define (sort-clauses clause*)
      (sort (lambda (cl1 cl2) (> (clause-count cl1) (clause-count cl2)))
        (map parse-clause clause*)))
    (define (reorder-cond clause* els?)
      #`(cond
          #,@(map clause-syn (sort-clauses clause*)) . #,els?))
      (syntax-case x (else)
      [(_ m1 ... (else e1 e2 ...)) (reorder-cond #'(m1 ...) #'([else e1 e2 ...]))]
      [(_ m1 ...) (reorder-cond #'(m1 ...) #'())])))
