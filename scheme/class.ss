(library (class)
  (export class new field (rename (method-profile-prob-inline method)))
  (import (chezscheme) (exclusive-cond) (utils))
  ;; A naïve implementation of a OO DSL.

  (meta define classes (make-hashtable equal-hash equal?))
  (define pi 3.14)
  (define (sqr n) (* n n))

  (define-syntax class
    (lambda (syn)
      (syntax-case syn (define-method)
        [(_ name ((field* val*) ...)
            (define-method (method* this* arg** ...) body** ...) ...)
         (let ([field-table (make-hashtable equal-hash equal?)]
               [method-table (make-hashtable equal-hash equal?)])
           (for-each (lambda (field val) (hashtable-set! field-table (syntax->datum field) val))
             #'(field* ...) #'(val* ...))
           (for-each (lambda (method body* arg*)
                       (hashtable-set! method-table (syntax->datum method)
                         (cons* #`(lambda #,arg* #,@body*) arg* body*)))
             #'(method* ...)
             #'((body** ...) ...)
             #'((this* arg** ...) ...))
           (hashtable-set! classes (syntax->datum #'name)
             (cons field-table method-table))
           ;; Have to return something?
           #'(define x (void)))])))

  (define-syntax field-set!
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj field val)
         #`(hashtable-set! obj 'field val)])))

  (define-syntax field
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj field)
         #`(hashtable-ref obj 'field #f)])))

  (define-syntax method
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj method arg* ...)
         #'((hashtable-ref obj 'method #f) obj arg* ...)])))

  (define-syntax new
    (lambda (syn)
      (syntax-case syn ()
        [(_ name)
         (let* ([class-tables (hashtable-ref classes (syntax->datum #'name) #f)]
                [fields (car class-tables)]
                [methods (cdr class-tables)])
           #`(let ([ht (make-hashtable equal-hash equal?)])
               (hashtable-set! ht 'class 'name)
               #,@(map
                    (lambda (field)
                      #`(hashtable-set! ht '#,(datum->syntax #'name field)
                          #,(hashtable-ref fields field #f)))
                    (vector->list (hashtable-keys fields)))
               #,@(map
                    (lambda (method)
                      #`(hashtable-set! ht '#,(datum->syntax #'name method)
                          #,(car (hashtable-ref methods method #f))))
                    (vector->list (hashtable-keys methods)))
               ht))])))


  ; --

  (define-syntax class-equal?
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj name)
         #'(equal? (hashtable-ref obj 'class #f) 'name)])))

  ;; naïve inline; check all classes in arbitrary order.
  (define-syntax method-inline
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj m val* ...)
         (with-syntax ([(this-val* ...) #'(obj val* ...)])
         #`(cond
             #,@(map (lambda (class)
                       (let* ([method-ht (cdr (hashtable-ref classes class #f))]
                               [method-info (hashtable-ref method-ht (syntax->datum #'m) #f)])
                          (with-syntax
                            ([(arg* ...) (cadr method-info)]
                             [(body* ...) (cddr method-info)])
                            #`[(class-equal? obj #,(datum->syntax #'obj class))
                              (let ([arg* this-val*] ...) body* ...)])))
                  (vector->list (hashtable-keys classes)))
              [else (method obj m val* ...)]))])))

  ;; better; check all classes in most likely order
  (define-syntax method-profile-inline
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj m val* ...)
         (with-syntax ([(this-val* ...) #'(obj val* ...)])
         #`(exclusive-cond
             #,@(map (lambda (class)
                       (let* ([method-ht (cdr (hashtable-ref classes class #f))]
                                [method-info (hashtable-ref method-ht (syntax->datum #'m) #f)])
                            (with-syntax
                              ([(arg* ...) (cadr method-info)]
                               [(body* ...) (cddr method-info)])
                              #`[(class-equal? obj #,(datum->syntax #'obj class))
                                (let ([arg* this-val*] ...) body* ...)])))
                  (vector->list (hashtable-keys classes)))
               [else (method obj m val* ...)]))])))

  ;; best; check likely classes in most likely order
  (define-syntax method-profile-prob-inline
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj m val* ...)
         (with-syntax ([(this-val* ...) #'(obj val* ...)])
           #`(exclusive-cond
               #,@(filter values
                    (map (lambda (class)
                           (let* ([method-ht (cdr (hashtable-ref classes class #f))]
                                  [method-info (hashtable-ref method-ht (syntax->datum #'m) #f)])
                                 (with-syntax
                                   ([(arg* ...) (cadr method-info)]
                                    [(body body* ...) (cddr method-info)])
                                   ;; Inline only the classes that take up
                                   ;; more than 20% of the computation.
                                   (if (> (or (profile-query-weight #'body) 0) .2)
                                       #`[(class-equal? obj #,(datum->syntax #'obj class))
                                          (let ([arg* this-val*] ...) body body* ...)]
                                       #f))))
                      (vector->list (hashtable-keys classes))))
               [else (method obj m val* ...)]))])))

  (let ()
    (class Square
      ((length 0) (width 0))
      (define-method (area this)
        (* (field this length) (field this width))))
    (class Circle
      ((radius 0))
      (define-method (area this)
        (* pi (sqr (field this radius)))))

    (define c (new Circle))

    (field-set! c radius 3)
    (assert (equal? (method c area) (* pi (sqr 3))))
    (assert (equal? (method c area) (* pi (sqr 3))))
    (assert (equal? (method-inline c area) (* pi (sqr 3))))
    (assert (equal? (method-profile-inline c area) (* pi (sqr 3))))
    (assert (equal? (method-profile-prob-inline c area) (* pi (sqr 3))))))
