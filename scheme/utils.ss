(library (utils)
  (export datum->annotated-syntax
          make-fresh-source-obj-factory!
          dummy-profile-query-weight)
  (import (chezscheme))

  (define (datum->annotated-syntax syn datum src)
    (let ([nsyn (datum->syntax syn datum)])
      (make-annotation nsyn src
        (cond [(syntax->annotation nsyn) => annotation-stripped]
              [else nsyn]))))

  (define (make-fresh-source-obj-factory! prefix)
    (let ([n -1])
      (lambda (syn)
        (let* ([sfd (make-source-file-descriptor (format "~a:~a:~a" prefix n n) #f)]
               [src (make-source-object n n sfd)])
          (set! n (sub1 n))
          src))))

  (define-syntax (dummy-profile-query-weight syn)
    (syntax-case syn ()
      [(_ arg* ...)
       (if (top-level-bound 'profile-query-weight)
           #'(profile-query-weight arg* ...)
           #'(random 1))])))
