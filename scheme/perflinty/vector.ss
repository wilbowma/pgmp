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
       vector?  vector-ref vector-copy vector-length vector-map
       vector-set!  vector->list)
     real:)
   (except (chezscheme)
     vector?  vector-ref vector-copy vector-length vector-map
     vector-set!  vector->list)
   ;(utils)
   ;; Use the dummy profile-query-weight function when running petite.
   (rename (utils) (dummy-profile-query-weight profile-query-weight)))

 (define-values
   (current-profiled-vector?
     current-profiled-vector-ref
     current-profiled-vector-copy
     current-profiled-vector-length
     current-profiled-vector-map
     current-profiled-vector-set!
     current-profiled-vector->list)
   (values
     (make-parameter real:vector?)
     (make-parameter real:vector-ref)
     (make-parameter real:vector-copy)
     (make-parameter real:vector-length)
     (make-parameter real:vector-map)
     (make-parameter real:vector-set!)
     (make-parameter real:vector->list)))

 (define-record vector-rep (finit vec))

 (define (vector? vec)
   ((vector-rep-finit vec))
         ((current-profiled-vector?) (vector-rep-vec vec)))
 (define (vector-ref vec)
   ((vector-rep-finit vec))
   ((current-profiled-vector-ref) (vector-rep-vec vec)))
 (define (vector-copy vec i)
   ((vector-rep-finit vec))
   (make-vector-rep (vector-rep-finit vec)
     ((current-profiled-vector-copy) (vector-rep-vec vec) i)))
 (define (vector-length vec)
   ((vector-rep-finit vec))
   ((current-profiled-vector-length) (vec)))
 ;; TODO: Find a way to support multiple vecs as input
 (define (vector-map f vec)
   ((vector-rep-finit vec))
   (make-vector-rep (vector-rep-finit vec)
     ((current-profiled-vector-map) f (vector-rep-vec vec))))
 ;; TODO: This might need to be a macro, as it kind of needs to generate
 ;; new sources
 (define (vector-set! vec p v)
   ((vector-rep-finit vec))
   (make-vector-rep
     (vector-rep-finit vec)
     ((current-profiled-vector-set!) (vector-rep-vec vec) p v)))
 (define (vector->list vec)
   ((vector-rep-finit vec))
   ((current-profiled-vector->list) (vector-rep-vec vec)))

(meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-vector"))
(meta define param* #'(current-profiled-vector?
  current-profiled-vector-ref current-profiled-vector-copy
  current-profiled-vector-length current-profiled-vector-map
  current-profiled-vector-set! current-profiled-vector->list))
(define-syntax (vector x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  ;; Defines all the sequences operations, giving profiled implementations
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
     (with-syntax ([(def* ...) op*]
                   [(name* ...) (generate-temporaries op*)]
                   [(param* ...) param*])
       #`(let ()
           (define name* def*) ...
           (make-vector-rep (lambda () (param* name*) ...)
             (real:vector init* ...))))])))
