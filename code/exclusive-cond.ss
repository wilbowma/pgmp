(define-syntax exclusive-cond
  (lambda (x)
    (define-record-type clause
      (nongenerative)
      (fields (immutable body) (immutable count))
      (protocol
        (lambda (new)
          (lambda (e1 e2)
            (new e1 (or (profile-query-count-syntax e2) 0))))))
    (define parse-clause
      (lambda (clause)
        (syntax-case clause (=>)
          #;[(e0) 
             (make-clause (lambda (next) #`(let ([t e0]) (if t t #,next))) 
               ???)]
          [(e0 => e1) 
           (make-clause (lambda (next) #`(let ([t e0]) (if t ((let ([p e1]) p) t) #,next)))
             #'e1)]
          [(e0 e1 e2 ...) 
           (make-clause (lambda (next) #`(if e0 (begin e1 e2 ...) #,next))
             #'e1)]
          [_ (syntax-error clause "invalid exclusive-cond clause")])))
    (define (helper clause* els) 
      (define (sort-em clause*)
        (sort (lambda (cl1 cl2) (> (clause-count cl1) (clause-count cl2))) 
          (map parse-clause clause*)))
      (define (emit clause* els)
        (fold-right (lambda (clause rest) ((clause-body clause) rest)) 
          els clause*))
      (emit (sort-em clause*) els))
    (syntax-case x (else)
      [(_ m1 ... (else e1 e2 ...)) (helper #'(m1 ...) #'(begin e1 e2 ...))]
      [(_ m1 ...) (helper #'(m1 ...) #'(void))])))
