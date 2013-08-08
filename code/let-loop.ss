(define-syntax named-let
  (lambda (x)
    (syntax-case x ()
      [(_ name ([x e] ...) b1 b2 ...)
       #'((letrec ([tmp (lambda (x ...)
                          (define-syntax name
                            (let ([unroll-count 1])
                              (lambda (q)
                                (syntax-case q ()
                                  [(_ enew (... ...))
                                   (if (= unroll-count 0)
                                       #'(tmp enew (... ...))
                                       (begin
                                         (set! unroll-count (- unroll-count 1))
                                         #'((lambda (x ...) b1 b2 ...) enew (... ...))))]))))
                          b1 b2 ...)])
            tmp)
          e ...)])))
