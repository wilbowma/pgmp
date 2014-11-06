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
    ;(utils)
    ;; Use the dummy profile-query-weight function when running petite.
    (rename (utils) (dummy-profile-query-weight profile-query-weight)))


  ;; TOOD: Boy does is this in need of a meta-program
  ;; Ought to write a macro that generates all these, call it in a
  ;; submodule, export all defined in it.
  ;;
  ;; maybe input:
  ;;  (list? : ((> ls))) (map : (f . (> lss))
  ;;  (car   : ((> ls))) (cdr : ((> ls)))
  ;;  (cons  : ((> ls))) (list-ref : ((> ls) n)) (length : ((> ls)))
  (define-values
    (current-profiled-length
     current-profiled-list?
     current-profiled-map
     current-profiled-car
     current-profiled-cdr
     current-profiled-cons
     current-profiled-list-ref)
    (values
      (make-parameter real:length)
      (make-parameter real:list?)
      (make-parameter real:map)
      (make-parameter real:car)
      (make-parameter real:cdr)
      (make-parameter real:cons)
      (make-parameter real:list-ref)))

  ;; TODO: Use this struct instead of implementing vectors as functions.
  (define-record list-rep (finit ls))

  (define (list? ls)
    ((list-rep-finit ls))
    ((current-profiled-list?) (list-rep-ls ls)))
  ;; TODO: This might need to be a macro, as it kind of needs to generate
  ;; new sources
  (define (map f ls)
    ((list-rep-finit ls))
    (make-list-rep (list-rep-finit ls) (apply (current-profiled-map) f (list-rep-ls ls))))
  (define (car ls)
    ((list-rep-finit ls))
    ((current-profiled-car) (list-rep-ls ls)))
  (define (cdr ls)
    ((list-rep-finit ls))
    ((current-profiled-cdr) (list-rep-ls ls)))
  ;; TODO: This might need to be a macro, as it kind of needs to generate
  ;; new sources
  (define (cons x ls)
    ((list-rep-finit ls))
    (make-list-rep (list-rep-finit ls) ((current-profiled-cons) x (list-rep-ls ls))))
  (define (list-ref ls n)
    ((list-rep-finit ls))
    ((current-profiled-list-ref) (list-rep-ls ls) n))
  (define (length ls)
    ((list-rep-finit ls))
    ((current-profiled-length) (list-rep-ls ls)))

  (meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-list"))
  (meta define param* #'(current-profiled-list?  current-profiled-map
    current-profiled-car current-profiled-cdr current-profiled-cons
    current-profiled-list-ref current-profiled-length))
  (define-syntax (list x)
    ;; Create fresh source object. list-src profiles operations that are
    ;; fast on lists, and vector-src profiles operations that are fast on
    ;; vectors.
    (define list-src (make-fresh-source-obj! x))
    (define vector-src (make-fresh-source-obj! x))
    ;; Defines all the sequences operations, giving profiled implementations
    (define op*
      (real:map
        (lambda (v src)
          (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
        '(real:list? real:map real:car real:cdr real:cons real:list-ref real:length)
        (real:list #f #f #f list-src list-src vector-src vector-src)))
    (syntax-case x ()
      [(_ init* ...)
       (unless (>= (profile-query-weight list-src) (profile-query-weight vector-src))
         (printf "WARNING: You should probably reimplement this list as a vector: ~a\n"
                 x))
       (with-syntax ([(def* ...) op*]
                     [(name* ...) (generate-temporaries op*)]
                     [(param* ...) param*])
         #`(let ()
             (define name* def*) ...
             (make-list-rep (lambda () (param* name*) ...)
                       (real:list init* ...))))])))
