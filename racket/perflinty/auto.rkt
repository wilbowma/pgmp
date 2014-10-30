#lang racket/base
(require
  (prefix-in real: racket/list)
  (prefix-in real: racket/vector)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
    "../profiling/utils.rkt"
    "../profiling/exact-interface.rkt"))
(provide
  seq?
  seq-map
  seq-first
  seq-rest
  seq-cons
  seq-append
  seq-copy
  seq-ref
  seq-set!
  seq
  seq-length
  ;; TODO: Is there a way to avoid exporting these? They should only be
  ;; called in this module, or by things generate by this module.
  real:length real:list real:list? real:map real:cons real:list-ref
  real:first real:rest real:append
  (prefix-out real:
    (combine-out
      list-copy
      list-set!))

  real:vector? real:vector-ref real:vector-copy real:vector-length
  real:vector-map real:vector-append real:vector-set!
  (prefix-out real:
    (combine-out
      vector-first
      vector-rest
      vector-cons))

  (prefix-out test:
    (combine-out
      seq-rep?
      seq-rep-s)))

(define (list-copy ls) (for/list ([i ls]) i))
(define (list-set! ls i v)
  (set! ls
    (let loop ([i i] [ls ls])
      (if (zero? i)
        (cons v (cdr ls))
        (cons (car ls) (loop (sub1 i) (cdr ls)))))))

(define (vector-first v) (real:vector-ref v 1))
(define (vector-rest v) (real:vector-drop v 1))
(define (vector-cons v vec)
  (real:vector-append (real:vector v) vec))
;; TODO: These should actually be undefined, or cause an error.
(define-values
  (current-profiled-seq?
   current-profiled-seq-map
   current-profiled-seq-first
   current-profiled-seq-rest
   current-profiled-seq-cons
   current-profiled-seq-append
   current-profiled-seq-copy
   current-profiled-seq-ref
   current-profiled-seq-set!
   current-profiled-seq
   current-profiled-seq-length)
  (values
    (make-parameter real:list?)
    (make-parameter real:map)
    (make-parameter real:first)
    (make-parameter real:rest)
    (make-parameter real:cons)
    (make-parameter real:append)
    (make-parameter list-copy)
    (make-parameter real:list-ref)
    (make-parameter list-set!)
    (make-parameter real:list)
    (make-parameter real:length)))

(struct seq-rep (finit s))

(define (seq? s)
  ((seq-rep-finit s))
  ((current-profiled-seq?) (seq-rep-s s)))

(define (seq-map f s)
  ((seq-rep-finit s))
  (seq-rep
    (seq-rep-finit s)
    ((current-profiled-seq-map) f (seq-rep-s s))))

(define (seq-first s)
  ((seq-rep-finit s))
  ((current-profiled-seq-first) (seq-rep-s s)))

(define (seq-rest s)
  ((seq-rep-finit s))
  (seq-rep
    (seq-rep-finit s)
    ((current-profiled-seq-rest) (seq-rep-s s))))

(define (seq-cons v s)
  ((seq-rep-finit s))
  (seq-rep
    (seq-rep-finit s)
    ((current-profiled-seq-cons) v (seq-rep-s s))))

;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define (seq-append s1 s2)
  ((seq-rep-finit s2))
  (seq-rep
    (seq-rep-finit s2)
    ((current-profiled-seq-append) (seq-rep-s s1) (seq-rep-s s2))))

(define (seq-copy s)
  ((seq-rep-finit s))
  (seq-rep
    (seq-rep-finit s)
    ((current-profiled-seq-copy) (seq-rep-s s))))

(define (seq-ref s p)
  ((seq-rep-finit s))
  ((current-profiled-seq-ref) (seq-rep-s s) p))

(define (seq-set! s p v)
  ((seq-rep-finit s))
  ((current-profiled-seq-set!) (seq-rep-s s) p v))

(define (seq-length s)
  ((seq-rep-finit s))
  ((current-profiled-seq-length) (seq-rep-s s)))

(begin-for-syntax
  (define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-sequence")))
(define-syntax (seq x)
  (define look-up-profile (load-profile x))
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  (define previous-list-usage (look-up-profile list-src))
  (define previous-vector-usage (look-up-profile vector-src))
  #;(displayln list-src)
  #;(displayln vector-src)
  #;(displayln previous-list-usage)
  #;(displayln previous-vector-usage)
  (define list>=vector (>= previous-list-usage previous-vector-usage))
  ;; Defines all the sequences operations, giving profiled implementations
  (define op*
    (map
      (lambda (v src)
        (datum->syntax x `(lambda args (apply ,v args)) (srcloc->list src)))
      (if list>=vector
        '(real:list? real:map real:first real:rest real:cons real:append
          real:list-copy real:list-ref real:list-set! real:length)
        '(real:vector? real:vector-map real:vector-first real:vector-rest
          real:vector-cons real:vector-append real:vector-copy
          real:vector-ref real:vector-set! real:vector-length))
      (list #f #f #f list-src list-src list-src #f vector-src vector-src
            vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (with-syntax ([(def* ...) op*]
                   [(name* ...) (generate-temporaries op*)]
                   [(params ...)
                    #'(current-profiled-seq?
                       current-profiled-seq-map
                       current-profiled-seq-first
                       current-profiled-seq-rest
                       current-profiled-seq-cons
                       current-profiled-seq-append
                       current-profiled-seq-copy
                       current-profiled-seq-ref
                       current-profiled-seq-set!
                       current-profiled-seq-length)])
       #`(let ()
           (define name* def*) ...
           (seq-rep (lambda ()
                      (params name*) ...)
                    (#,(if list>=vector
                         #'real:list
                         #'real:vector) init* ...))))]))
