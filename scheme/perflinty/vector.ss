(library (perflinty vector)
 (export
   vector?
   vector-ref
   vector-copy
   vector-length
   vector-map
   vector-set!
   vector->list
   ;list->vector
   ;; TODO: Is there a way to avoid exporting these? They should only be
   ;; called in this module, or by things generate by this module.
   real:vector? real:vector-ref real:vector-copy real:vector-length
   real:vector-map real:vector-set!
   real:vector->list ;real:list->vector
   vector)
 (import
   (prefix
     (only
       (chezscheme)
       vector vector?  vector-ref vector-copy vector-length vector-map
       vector-set!  vector->list)
     real:)
   (except (chezscheme)
     vector vector?  vector-ref vector-copy vector-length vector-map
     vector-set!  vector->list)
   ;(utils)
   ;; Use the dummy profile-query-weight function when running petite.
   (rename (utils) (dummy-profile-query-weight profile-query-weight)))

  ;; NB This representation has some overhead. Need to find a way
  ;; NB to make more of this happen at compile time.
  (define-record vector-rep (op-table vec))
  (define-syntax (define-vector-rep-op syn)
   (syntax-case syn ()
     ;; v must appear in args ...
     [(_ name v (arg* ...))
      #`(define (name #,@(map (lambda (arg)
                                (syntax-case arg (rep)
                                  [(rep vec) #'vec]
                                  [_ arg]))
                           (syntax->list #'(arg* ...))))
          (make-vector-rep (vector-rep-op-table v)
            ((hashtable-ref (vector-rep-op-table v) 'name #f)
             #,@(map (lambda (arg)
                       (syntax-case arg (rep)
                         [(rep vec) #'(vector-rep-vec vec)]
                         [_ arg]))
                  (syntax->list #'(arg* ...))))))]))

  (define-vector-rep-op vector? vec ([rep vec]))
  (define-vector-rep-op vector-ref vec ([rep vec] pos))
  (define-vector-rep-op vector-copy vec ([rep vec] i))
  (define-vector-rep-op vector-length vec ([rep vec]))
  ;; TODO: Find a way to support multiple vecs as input
  (define-vector-rep-op vector-map vec (f [rep vec]))
  ;; TODO: This might need to be a macro, as it kind of needs to generate
  ;; new sources
  (define-vector-rep-op vector-append vec2 ([rep vec1] [rep vec2]))
  (define-vector-rep-op vector-set! vec ([rep vec] p v))
  (define-vector-rep-op vector->list vec ([rep vec]))
  
  (meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-vector"))
  
  (define-syntax (vector x)
    ;; Create fresh source object. list-src profiles operations that are
    ;; fast on lists, and vector-src profiles operations that are fast on
    ;; vectors.
    (define list-src (make-fresh-source-obj! x))
    (define vector-src (make-fresh-source-obj! x))
    ;; Defines all the sequences operations, giving profiled implementations
    (define op-name* '(vector? vector-ref vector-copy vector-length
      vector-map vector-append vector-set! vector->list))
    (define op*
      (map
        (lambda (v src)
          (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
        '(real:vector? real:vector-ref real:vector-copy real:vector-length
          real:vector-map real:vector-set! real:vector->list)
        (list #f vector-src list-src vector-src list-src list-src vector-src)))
    (syntax-case x ()
      [(_ init* ...)
       (unless (>= (profile-query-weight vector-src) (profile-query-weight list-src))
               (printf "WARNING: You should probably reimplement this vector as a list: ~a\n" x))
       #`(let ()
           (make-vector-rep
             (let ([ht (make-eq-hashtable)])
                  #,@(map (lambda (op op-name) #`(hashtable-set! ht #,op-name #,op))
                       (syntax->list op*) (syntax->list op-name*))
                  ht)
             (real:vector init* ...)))])))
