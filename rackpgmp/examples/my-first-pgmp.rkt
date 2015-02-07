#lang racket
(require pgmp)
(provide if-r)

(define-syntax (if-r stx)
  (define profile-query
    (let ([f (load-profile-query-weight stx)])
      (lambda (x) (or (f x) 0))))
  (syntax-case stx ()
    [(_ test t f)
     (let ([t-prof (profile-query #'t)]
           [f-prof (profile-query #'f)])
       (if (< t-prof f-prof)
           #'(if (not test) f t)
           #'(if test       t f)))]))

(module+ test
  (require rackunit syntax/srcloc racket/serialize)
  (check-equal?
    '(if (subject-contains email "PLDI")
         (flag email 'important)
         (flag email 'spam))
    ;; Perform one expansion, then convert the resulting syntax to a
    ;; list
    (syntax->datum
     (expand-once
       #'(if-r (subject-contains email "PLDI")
               (flag email 'important)
               (flag email 'spam)))))

  ;; Fake some profile points
  ;; I'm breaking abstraction a little here. I happen to know that
  ;; srclocs are valid profile-points.
  (define profile-point-t (srcloc "my-first-pgmp" 1 1 1 1))
  (define profile-point-f (srcloc "my-first-pgmp" 2 1 1 1))

  ;; Generate some profile information.
  ;; I'm breaking abstraction a little here---I happen to know that
  ;; profile information is stored as a list of pairs of profile-points
  ;; and numbers.
  (with-output-to-file "my-first-pgmp.rkt.profile"
    (thunk
      (write (serialize (list (cons profile-point-t 10)
                              (cons profile-point-f 20)))))
    #:exists 'replace)

  (check-equal?
    '(if (not (subject-contains email "PLDI"))
         (flag email 'spam)
         (flag email 'important))
    (syntax->datum
     (expand-once
       #`(if-r (subject-contains email "PLDI")
             ;; Annotate the branches with our new profile points
             #,(annotate-syn
                 profile-point-t
                 (flag email 'important))
             #,(annotate-syn
                 profile-point-f
                 (flag email 'spam)))))))
