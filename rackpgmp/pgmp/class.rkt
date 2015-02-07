#lang racket/base

(require
  (only-in racket/math pi sqr)
  (only-in rnrs make-hashtable hashtable-set! hashtable-ref equal-hash
           hashtable-keys)
  (for-syntax "profiling/exact-interface.rkt")
  (for-syntax (except-in racket class new field)))
(provide class field method new field-set! class-equal?
         inspect-receiver-class-list)

(define inspect-receiver-class-list (make-parameter '()))
(begin-for-syntax (define classes (make-hashtable equal-hash equal?)))

(define-syntax class
  (lambda (syn)
    (syntax-case syn (define-method)
      [(_ name ((field* val*) ...)
          (define-method (method* this* arg** ...) body** ...) ...)
       (let ([field-table (make-hashtable equal-hash equal?)]
             [method-table (make-hashtable equal-hash equal?)])
         (for-each (lambda (field val) (hashtable-set! field-table field val))
           (syntax->datum #'(field* ...)) (syntax->list #'(val* ...)))
         (for-each (lambda (method body* arg*)
                     (hashtable-set! method-table method
                       (list* #`(lambda #,arg* #,@body*) arg* body*)))
           (syntax->datum #'(method* ...))
           (syntax->list #'((body** ...) ...))
           (syntax->list #'((this* arg** ...) ...)))
         (hashtable-set! classes (syntax->datum #'name)
           (cons field-table method-table))
         #'(void))])))

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

(begin-for-syntax
  (define (build-dynamic-dispatch loc obj method arg*)
    (annotate-syn loc
      (let ([x #,obj]) ((hashtable-ref x '#,method #f) x #,@arg*)))))
(define-syntax dynamic-dispatch
  (lambda (syn)
    (syntax-case syn ()
      [(_ obj method arg* ...)
       (build-dynamic-dispatch syn #'obj #'method #'(arg* ...))])))

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
                    #`(hashtable-set! ht '#,field
                        #,(hashtable-ref fields field #f)))
                  (vector->list (hashtable-keys fields)))
             #,@(map
                  (lambda (method)
                    #`(hashtable-set! ht '#,method
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
;; Strategy: manufactor new source objects for each class used
;; at this class site. How? Create new source objects, one for each
;; class based on this syn, and install them to be counted conditionally
;; in method
(begin-for-syntax
  (define make-profile-point (make-profile-point-factory "method-call")))
(define-syntax method
  (lambda (syn)
    (define profile-query-weight (load-profile-query-weight syn))
    (define class-list (vector->list (hashtable-keys classes)))
    (define srclocs (map (lambda (x) (make-profile-point syn)) class-list))
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
      [(_ obj m val* ...)
       (with-syntax ([(arg* ...) (generate-temporaries (syntax->datum #'(val* ...)))])
       (let* ([instrumented-dispatch
                (map (lambda (loc)
                      #`(lambda (this arg* ...)
                          #,(build-dynamic-dispatch loc #'obj #'m #'(arg* ...))))
                     srclocs)]
             [_sorted-class*weights (sort (map (λ (x class) (cons class (profile-query-weight x)))
                                              srclocs class-list)
                                    (λ (x y) (> (or (cdr x) 0) (or (cdr y) 0))))]
             [no-profile-data? (not (ormap cdr _sorted-class*weights))]
             [sorted-classes (map car _sorted-class*weights)]
             [sorted-weights (map cdr _sorted-class*weights)])
            #`(let* ([x obj])
                  (when (null? (inspect-receiver-class-list))
                    (inspect-receiver-class-list '#,(take sorted-classes inline-limit)))
                  (cond
                    #,@(if no-profile-data?
                           (for/list ([d instrumented-dispatch] [cls class-list])
                             #`((class-equal? x #,(datum->syntax syn cls))
                                (#,d x val* ...)))
                           (for/list ([class (take sorted-classes inline-limit)]
                                      [weight (take sorted-weights inline-limit)])
                           #`((class-equal? x #,(datum->syntax syn class))
                              #,(inline-method class #'m #'x #'(val* ...)))))))))])))

(module+ test
  (require rackunit)
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
  (check-true (class-equal? c Circle))
  (check-true (class-equal? s Square))
  (check-equal? (dynamic-dispatch c area) (* pi (sqr 3)))
  (check-equal? (method c area) (* pi (sqr 3)))
  (check-equal? (dynamic-dispatch s area) (sqr 3))
  (check-equal? (method s area) (sqr 3)))
