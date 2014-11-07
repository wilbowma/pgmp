(define-syntax named-let
  (lambda (x)
    (syntax-case x ()
      [(_ name ([x e] ...) b1 b2 ...)
       #`((letrec ([tmp (lambda (x ...)
                          #,(let* ([profile-weight (or (profile-query-count-syntax #'b1) 0)]
                                   [unroll-limit (+ 1 (* 3 (/ profile-weight 1000)))])
                              #`(define-syntax name
                                  (let ([unroll-count #,unroll-limit]
                                        [weight #,profile-weight])
                                    (lambda (q)
                                      (syntax-case q ()
                                        [(_ enew (... ...))
                                          (if (or (= unroll-count 0)
                                                  (< weight 100))
                                              #'(tmp enew (... ...))
                                              (begin
                                                (set! unroll-count (- unroll-count 1))
                                                (display "unrolled") (newline)
                                                #'((lambda (x ...) b1 b2 ...) enew (... ...))))])))))

                          b1 b2 ...)])
            tmp)
          e ...)])))

(define (test)
  (named-let fact ([n 5])
    (case n
      [0 1]
      [else (* n (fact (sub1 n)))])))
