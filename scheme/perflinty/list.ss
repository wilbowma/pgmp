(library (perflinty list)
  (export
    list?
    map
    car
    cdr
    cons
    list-ref
    list
    length
    ;; TODO: Is there a way to avoid exporting these? They should only be
    ;; called in this module, or by things generate by this module.
    real:length real:list real:list? real:map real:car real:cdr real:cons real:list-ref)
  (import
    (prefix
     (only
       (chezscheme)
       length list list? map car cdr cons list-ref)
     real:)
    (except (chezscheme)
      length list list? map car cdr cons list-ref)
    (rename (utils) (dummy-profile-query-weight profile-query-weight)))

  (define-record list-rep (op-table ls))

  (define-syntax (define-list-rep-op syn)
    (syntax-case syn ()
      ;; ls must appear in args ...
      [(_ name ls-rep (arg* ...))
       #`(define (name #,@(real:map (lambda (arg)
                                 (syntax-case arg (rep)
                                    [(rep a) #'a]
                                    [_ arg]))
                               (syntax->list #'(arg* ...))))
           (make-list-rep (list-rep-op-table ls-rep)
             ((hashtable-ref (list-rep-op-table ls-rep) 'name #f)
              #,@(real:map (lambda (arg)
                        (syntax-case arg (rep)
                          [(rep a) #'(list-rep-ls a)]
                          [_ arg]))
                      (syntax->list #'(arg* ...))))))]))

  (define-list-rep-op list? ls ([rep ls]))
  ;; TODO: This might need to be a macro, as it kind of needs to generate
  ;; new sources
  (define-list-rep-op map ls (f [rep ls]))
  (define-list-rep-op car ls ([rep ls]))
  (define-list-rep-op cdr ls ([rep ls]))
  ;; TODO: This might need to be a macro, as it kind of needs to generate
  ;; new sources
  (define-list-rep-op cons ls (x [rep ls]))
  (define-list-rep-op list-ref ls ([rep ls] n))
  (define-list-rep-op length ls ([rep ls]))

  (meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-list"))
  (define-syntax (list x)
    ;; Create fresh source object. list-src profiles operations that are
    ;; fast on lists, and vector-src profiles operations that are fast on
    ;; vectors.
    (define list-src (make-fresh-source-obj! x))
    (define vector-src (make-fresh-source-obj! x))
    ;; Defines all the sequences operations, giving profiled implementations
    (define op-name* '(list? map car cdr cons list-ref length))
    (define op*
      (real:map
        (lambda (v src)
          (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
        '(real:list? real:map real:car real:cdr real:cons real:list-ref real:length)
        (real:list #f #f #f list-src list-src vector-src vector-src)))
    (syntax-case x ()
      [(_ init* ...)
       (unless (>= (profile-query-weight list-src) (profile-query-weight vector-src))
         (printf "WARNING: You should probably reimplement this list as a vector: ~a\n" x))
        #`(let ()
            (make-list-rep
              (let ([ht (make-eq-hashtable)])
                #,@(real:map (lambda (op op-name) #`(hashtable-set! ht #,op-name #,op))
                     (syntax->list op*) (syntax->list op-name*))
                ht)
              (real:list init* ...)))])))
