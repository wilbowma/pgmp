(library (class)
  (export class new field method field-set!)
  (import (chezscheme) (exclusive-cond) (utils))
  ;; A naïve implementation of a OO DSL.

  (meta define classes (make-hashtable equal-hash equal?))
  (define pi 3.14)
  (define (sqr n) (* n n))
  (meta define (take ls n)
    (do ([i n (sub1 i)]
         [ls ls (cdr ls)]
         [nls '() (cons (car ls) nls)])
        ((zero? i) nls)))

  (define-syntax class
    (lambda (syn)
      (syntax-case syn (define-method)
        [(_ name ((field* val*) ...)
            (define-method (method* this* arg** ...) body** ...) ...)
         (let ([field-table (make-hashtable equal-hash equal?)]
               [method-table (make-hashtable equal-hash equal?)])
           (for-each (lambda (field val) (hashtable-set! field-table field val))
             (syntax->datum #'(field* ...)) #'(val* ...))
           (for-each (lambda (method body* arg*)
                       (hashtable-set! method-table method
                         (cons* #`(lambda #,arg* #,@body*) arg* body*)))
             (syntax->datum #'(method* ...))
             #'((body** ...) ...)
             #'((this* arg** ...) ...))
           (hashtable-set! classes (syntax->datum #'name)
             (cons field-table method-table))
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

  (define-syntax dynamic-dispatch
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj method arg* ...)
         #'(let ([x obj]) ((hashtable-ref x 'method #f) x arg* ...))])))

  (define-syntax new
    (lambda (syn)
      (syntax-case syn ()
        [(id name)
         (let* ([class-tables (hashtable-ref classes (syntax->datum #'name) #f)]
                [fields (car class-tables)]
                [methods (cdr class-tables)])
           #`(let ([ht (make-hashtable equal-hash equal?)])
               (hashtable-set! ht 'class 'name)
               #,@(map
                    (lambda (field)
                      #`(hashtable-set! ht '#,(datum->syntax #'id field)
                          #,(hashtable-ref fields field #f)))
                    (vector->list (hashtable-keys fields)))
               #,@(map
                    (lambda (method)
                      #`(hashtable-set! ht '#,(datum->syntax #'id method)
                          #,(car (hashtable-ref methods method #f))))
                    (vector->list (hashtable-keys methods)))
               ht))])))

  ; --

  (define-syntax class-equal?
    (lambda (syn)
      (syntax-case syn ()
        [(_ obj name)
         #'(eq? (hashtable-ref obj 'class #f) 'name)])))

  ;; A receiver-class prediction implementation of method calls
  ;; Strategy: manufactory new source objects for each class used
  ;; at this class site. How? Create new source objects, one for each
  ;; class based on this syn, and install them to be counted conditionally
  ;; in method
  (meta define make-fresh-source-obj! (make-fresh-source-obj-factory!  "method-call"))
  (define-syntax method
    (lambda (syn)
      (define class-list (vector->list (hashtable-keys classes)))
      (define srclocs (map (lambda (x) (make-fresh-source-obj! syn)) class-list))
      (define inline-limit (min 5 (length class-list)))
      (define (inline-method class method obj vals)
       (let* ([method-ht (cdr (hashtable-ref classes class #f))]
              [method-info (hashtable-ref method-ht (syntax->datum method) #f)])
         (with-syntax
           ([(arg* ...) (cadr method-info)]
            [(body body* ...) (cddr method-info)]
            [(this-val* ...) `(,obj . ,vals)])
           #`(let ([arg* this-val*] ...) body body* ...))))
      (syntax-case syn ()
        [(id obj m val* ...)
         (let* ([instrumented-dispatch
                (map (lambda (loc)
                        (datum->annotated-syntax #'id `(lambda (this . args) (apply
                                                                    (hashtable-ref
                                                                      this
                                                                      ',#'m #f)
                                                                    this args))
                                       loc))
                     srclocs)]
             [_sorted-class*weights (sort (lambda (x y) (> (or (cdr x) 0) (or (cdr y) 0)))
                                          (map (lambda (x class) (cons class (profile-query-weight x)))
                                              srclocs class-list))]
             [no-profile-data? (not (ormap cdr _sorted-class*weights))]
             [sorted-classes (map car _sorted-class*weights)]
             [sorted-weights (map cdr _sorted-class*weights)])
            (with-syntax ([(arg* ...) (generate-temporaries (syntax->datum #'(val* ...)))])
            #`(let* ([x obj])
                (cond
                  #,@(if no-profile-data?
                         (map (lambda (d cls)
                                #`((class-equal? x #,(datum->syntax #'id cls))
                                   (#,d x val* ...)))
                              instrumented-dispatch class-list)
                         (map (lambda (class weight)
                                (printf "Class ~a has weight ~a at call site ~a\n" class
                                  weight (syntax->annotation syn))
                                #`((class-equal? x #,(datum->syntax #'id class))
                                   #,(inline-method class #'m #'x #'(val* ...))))
                              (take sorted-classes inline-limit)
                              (take sorted-weights inline-limit)))))))])))

  (let ()
    (class Square
      ((length 0))
      (define-method (area this)
        (sqr (field this length))))
    (class Circle
      ((radius 0))
      (define-method (area this)
        (* pi (sqr (field this radius)))))

    (define c (new Circle))
    (define s (new Square))

    (field-set! c radius 3)
    (field-set! s length 3)
    (assert (class-equal? s Square))
    (assert (equal? (dynamic-dispatch c area) (* pi (sqr 3))))
    (assert (equal? (method c area) (* pi (sqr 3))))
    #;(assert (equal? (dynamic-dispatch s area) (sqr 3)))
    #;(assert (equal? (method s area) (sqr 3)))))
