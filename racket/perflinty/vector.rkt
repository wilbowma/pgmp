#lang racket/base
(require
  racket/trace
  (prefix-in real: racket/vector)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
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
  list->vector
  ;; TODO: Is there a way to avoid exporting these? They should only be
  ;; called in this module, or by things generate by this module.
  real:vector? real:vector-ref real:vector-copy real:vector-length
  real:vector-map real:vector-append real:vector-set!
  real:vector->list real:list->vector
  vector)

#;(module profiled-list racket/base
  )
;; TOOD: Boy does is this in need of a meta-program
;; Ought to write a macro in that generates all these, call it in a
;; submodule, export all defined in it.
;;
;; maybe input:
;;  (list? : ((> ls))) (map : (f . (> lss))
;;  (car   : ((> ls))) (cdr : ((> ls)))
;;  (cons  : ((> ls))) (list-ref : ((> ls) n)) (length : ((> ls)))
(define-values
  (current-profiled-vector?
   current-profiled-vector-ref
   current-profiled-vector-copy
   current-profiled-vector-length
   current-profiled-vector-map
   current-profiled-vector-append
   current-profiled-vector-set!
   current-profiled-vector->list
   current-profiled-list->vector)
  (values
    (make-parameter real:vector?)
    (make-parameter real:vector-ref)
    (make-parameter real:vector-copy)
    (make-parameter real:vector-length)
    (make-parameter real:vector-map)
    (make-parameter real:vector-append)
    (make-parameter real:vector-set!)
    (make-parameter real:vector->list)
    (make-parameter real:list->vector)))

;; TODO: Use this struct instead of implementing vectors as functions.
(struct (vector-rep finit vec))
;; These are delicate; profiled list must evaluated first to set parameters.
;; But Racket is CBV so it's okay for now.
(define (vector? vec)

  ((current-profiled-vector?) (vec)))
(define (vector-ref vec)
  ((current-profiled-vector-ref) (vec)))
(define (vector-copy vec i)
  ((current-profiled-vector-copy) (vec) i))
(define (vector-length vec)
  ((current-profiled-vector-length) (vec)))
;; TODO: These may not work right... probably only one version of the
;; parameters will get set.
;; TODO Functions that return vectors don't work right.
(trace-define (vector-map f . vec)
  (apply (current-profiled-vector-map) f (real:map (lambda (v) (v)) vec)))
(trace-define (vector-append . vec)
  (apply (current-profiled-vector-append) (real:map (lambda (v) (v)) vec)))
(define (vector-set! vec p v)
  ((current-profiled-vector-set!) (vec) p v))
(define (vector->list vec)
  ((current-profiled-vector->list) (vec)))
;; Expects a non-profiled list. ...
(define (list->vector ls)
  ((current-profiled-list->vector) (vector ls)))

(begin-for-syntax
  (define (srcloc->list srcloc)
    (and srcloc
         (list (srcloc-source srcloc)
           (srcloc-line srcloc)
           (srcloc-column srcloc)
           (srcloc-position srcloc)
           (srcloc-span srcloc))))
  (define make-fresh-source-obj!
    (let ([n 0])
      (lambda (syn)
        (let* ([src (make-srcloc (format "profiled-vector~s" n)
                                 (syntax-line syn)
                                 (syntax-column syn)
                                 (syntax-position syn)
                                 (syntax-span syn))])
          (set! n (add1 n))
          src)))))

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
        real:vector->list real:list->vector)
      (list #f vector-src list-src vector-src list-src list-src
            vector-src list-src vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (look-up-profile vector-src) (look-up-profile list-src))
       (printf "WARNING: You should probably reimplement this vector as a list: ~a"
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
                       current-profiled-list->vector)])
       #`(let ()
           (define name* def*) ...
           (vector-rep
             (lambda () (params name*) ...)
             (real:vector init* ...))))]))
