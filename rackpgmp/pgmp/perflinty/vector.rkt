#lang racket/base
(require
  racket/trace
  (prefix-in real: racket/vector)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
    "../profiling/exact-interface.rkt"))
(provide
  vector
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
  real:vector->list)

(struct vector-rep (op-table vec))

(define-syntax (define-vector-rep-op syn)
  (syntax-case syn ()
    ;; v must appear in args ...
    [(_ name v (arg* ...))
     #`(define (name #,@(map (λ (arg)
                                (syntax-case arg (rep)
                                  [(rep vec) #'vec]
                                  [_ arg]))
                             (syntax->list #'(arg* ...))))
         (struct-copy vector-rep v
           [vec ((hash-ref (vector-rep-op-table v) 'name)
                #,@(map (λ (arg)
                           (syntax-case arg (rep)
                             [(rep vec) #'(vector-rep-vec vec)]
                             [_ arg]))
                    (syntax->list #'(arg* ...))))]))]))

(define-vector-rep-op vector? vec ([rep vec]))
(define-vector-rep-op vector-copy vec ([rep vec] i))
(define-vector-rep-op vector-length vec ([rep vec]))
;; TODO: Find a way to support multiple vecs as input
(define-vector-rep-op vector-map vec (f [rep vec]))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define-vector-rep-op vector-append vec2 ([rep vec1] [rep vec2]))
;; TODO: ref and set! should probably count higher indecies cheaper, but
;; that would require more clever engineering.
(define-vector-rep-op vector-ref vec ([rep vec] pos))
(define-vector-rep-op vector-set! vec ([rep vec] p v))
(define-vector-rep-op vector->list vec ([rep vec]))

(begin-for-syntax
  (define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-vector")))
(define-syntax (vector x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define profile-query-weight (load-profile-query-weight x))
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  ;; Defines all the sequences operations, giving profiled implementations
  (define op-name* '(vector? vector-ref vector-copy vector-length
    vector-map vector-append vector-set! vector->list))
  (define op*
    (map
      (lambda (v src)
        (datum->syntax x `(lambda args (apply ,v args)) (srcloc->list src)))
      '(real:vector? real:vector-ref real:vector-copy real:vector-length
        real:vector-map real:vector-append real:vector-set!
        real:vector->list)
      (list #f vector-src list-src vector-src list-src list-src
            vector-src list-src)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (or (profile-query-weight vector-src) 0)
                 (or (profile-query-weight list-src) 0))
       (printf "WARNING: You should probably reimplement this vector as a list: ~a\n" x))
     (with-syntax ([(op* ...) op*] [(op-name* ...) op-name*])
       #`(let ()
           (vector-rep
             (make-immutable-hasheq (real:list (real:cons 'op-name* op*) ...))
             (real:vector init* ...))))]))
