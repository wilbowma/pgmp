#lang racket/base
(require
  racket/trace
  (prefix-in real: racket/vector)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
    "../profiling/utils.rkt"
    "../profiling/exact-interface.rkt"))
(provide
  vector?
  vector-ref
  vector-copy
  vector-length
  vector-map
  vector-append
  vector-set!
  vector->list
  ;list->vector
  ;; TODO: Is there a way to avoid exporting these? They should only be
  ;; called in this module, or by things generate by this module.
  real:vector? real:vector-ref real:vector-copy real:vector-length
  real:vector-map real:vector-append real:vector-set!
  real:vector->list ;real:list->vector
  vector)

(define-values
  (current-profiled-vector?
   current-profiled-vector-ref
   current-profiled-vector-copy
   current-profiled-vector-length
   current-profiled-vector-map
   current-profiled-vector-append
   current-profiled-vector-set!
   current-profiled-vector->list
   #;current-profiled-list->vector)
  (values
    (make-parameter real:vector?)
    (make-parameter real:vector-ref)
    (make-parameter real:vector-copy)
    (make-parameter real:vector-length)
    (make-parameter real:vector-map)
    (make-parameter real:vector-append)
    (make-parameter real:vector-set!)
    (make-parameter real:vector->list)
    #;(make-parameter real:list->vector)))

(struct vector-rep (finit vec))

(define (vector? vec)
  ((vector-rep-finit vec))
  ((current-profiled-vector?) (vector-rep-vec vec)))
(define (vector-ref vec)
  ((vector-rep-finit vec))
  ((current-profiled-vector-ref) (vector-rep-vec vec)))
(define (vector-copy vec i)
  ((vector-rep-finit vec))
  (vector-rep (vector-rep-finit vec)
              ((current-profiled-vector-copy) (vector-rep-vec vec) i)))
(define (vector-length vec)
  ((vector-rep-finit vec))
  ((current-profiled-vector-length) (vec)))
;; TODO: Find a way to support multiple vecs as input
(define (vector-map f vec)
  ((vector-rep-finit vec))
  (vector-rep (vector-rep-finit vec)
              ((current-profiled-vector-map) f (vector-rep-vec vec))))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define (vector-append vec1 vec2)
  ((vector-rep-finit vec2))
  (vector-rep
    (vector-rep-finit vec2)
    ((current-profiled-vector-append) (vector-rep-vec vec1) (vector-rep-vec vec2))))
(define (vector-set! vec p v)
  ((vector-rep-finit vec))
  (vector-rep
    (vector-rep-finit vec)
    ((current-profiled-vector-set!) (vector-rep-vec vec) p v)))
(define (vector->list vec)
  ((vector-rep-finit vec))
  ((current-profiled-vector->list) (vector-rep-vec vec)))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
;; Expects a non-profiled list. ...
#;(define (list->vector ls)
  ((current-profiled-list->vector) (vector ls)))

(begin-for-syntax
  (define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-vector")))
(define-syntax (vector x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define look-up-profile (load-profile x))
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  ;; Defines all the sequences operations, giving profiled implementations
  (define op*
    (map
      (lambda (v src)
        (datum->syntax x `(lambda args (apply ,v args)) (srcloc->list src)))
      '(real:vector? real:vector-ref real:vector-copy real:vector-length
        real:vector-map real:vector-append real:vector-set!
        real:vector->list #;real:list->vector)
      (list #f vector-src list-src vector-src list-src list-src
            vector-src list-src #;vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (look-up-profile vector-src) (look-up-profile list-src))
       (printf "WARNING: You should probably reimplement this vector as a list: ~a\n"
               x))
     (with-syntax ([(def* ...) op*]
                   [(name* ...) (generate-temporaries op*)]
                   [(params ...)
                    #'(current-profiled-vector?
                       current-profiled-vector-ref
                       current-profiled-vector-copy
                       current-profiled-vector-length
                       current-profiled-vector-map
                       current-profiled-vector-append
                       current-profiled-vector-set!
                       current-profiled-vector->list
                       #;current-profiled-list->vector)])
       #`(let ()
           (define name* def*) ...
           (vector-rep
             (lambda () (params name*) ...)
             (real:vector init* ...))))]))
