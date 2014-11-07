(library (utils)
  (export datum->annotated-syntax
          make-fresh-source-obj-factory!
          (rename (dummy-profile-query-weight profile-query-weight)))
  (import (chezscheme))

  (define (datum->annotated-syntax syn datum src)
    (let ([nsyn (datum->syntax syn datum)])
      (make-annotation nsyn src
        (cond [(syntax->annotation nsyn) => annotation-stripped]
              [else nsyn]))))

  (define (syntax->filename syn)
    (cond
      [(syntax->annotation syn) =>
       (lambda (a) (source-file-descriptor-path (source-object-sfd (annotation-source a))))]
      [else ""]))

  (define (make-fresh-source-obj-factory! prefix)
    (let ([n 0])
      (lambda (syn)
        (let* ([sfd (make-source-file-descriptor
                      (format "~a:~a:~a" (syntax->filename syn) prefix n) #f)]
               [src (make-source-object n n sfd)])
          (set! n (add1 n))
          src))))

  (define-syntax (dummy-profile-query-weight syn)
    (syntax-case syn ()
      [(_ arg* ...)
       (if (top-level-bound? 'profile-query-weight)
           #'(profile-query-weight arg* ...)
           #'(random 1))])))
