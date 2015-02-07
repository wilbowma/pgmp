#lang racket/base
(require
  racket/list
  racket/vector
  (for-syntax
    racket/base
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

  list-copy
  list-set!
  vector-first
  vector-rest
  vector-cons

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

(define (vector-first v) (vector-ref v 1))
(define (vector-rest v) (vector-drop v 1))
(define (vector-cons v vec)
  (vector-append (vector v) vec))

(struct seq-rep (op-table s))

(define-syntax (define-seq-rep-op syn)
  (syntax-case syn ()
    ;; s must appear in args ...
    [(_ name seq (arg* ...))
     #`(define (name #,@(map (λ (arg)
                                (syntax-case arg (rep)
                                  [(rep a) #'a]
                                  [_ arg]))
                             (syntax->list #'(arg* ...))))
         (struct-copy seq-rep seq
           [s ((hash-ref (seq-rep-op-table seq) 'name)
                #,@(map (λ (arg)
                           (syntax-case arg (rep)
                             [(rep a) #'(seq-rep-s a)]
                             [_ arg]))
                    (syntax->list #'(arg* ...))))]))]))

(define-seq-rep-op seq? s ([rep s]))
(define-seq-rep-op seq-map s (f [rep s]))
(define-seq-rep-op seq-first s ([rep s]))
(define-seq-rep-op seq-rest s ([rep s]))
(define-seq-rep-op seq-cons s (v [rep s]))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define-seq-rep-op seq-append s2 ([rep s1] [rep s2]))
(define-seq-rep-op seq-copy s ([rep s]))
;; TODO: ref and set! should probably count higher indecies more expensive, but
;; that would require more clever engineering.
(define-seq-rep-op seq-ref s ([rep s] p))
(define-seq-rep-op seq-set! s ([rep s] p v))
(define-seq-rep-op seq-length s ([rep s]))

(begin-for-syntax
  (define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-sequence")))
(define-syntax (seq x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define profile-query-weight (load-profile-query-weight x))
  (define list-src (make-fresh-source-obj! x))
  (define vector-src (make-fresh-source-obj! x))
  (define previous-list-usage (or (profile-query-weight list-src) 0))
  (define previous-vector-usage (or (profile-query-weight vector-src) 0))
  (define list>=vector (>= previous-list-usage previous-vector-usage))
  ;; Defines all the sequences operations, giving profiled implementations
  (define op-name* '(seq? seq-map seq-first seq-rest seq-cons seq-append
    seq-copy seq-ref seq-set! seq-length))
  (define op*
    (map
      (lambda (v src)
        (datum->syntax x `(lambda args (apply ,v args)) (srcloc->list src)))
      (if list>=vector
        '(list? map first rest cons append list-copy list-ref list-set!
          length)
        '(vector? vector-map vector-first vector-rest vector-cons
          vector-append vector-copy vector-ref vector-set!
          vector-length))
      (list #f #f list-src list-src list-src list-src #f vector-src vector-src
            vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (with-syntax ([(op* ...) op*] [(op-name* ...) op-name*])
       #`(let ()
           (seq-rep
             (make-immutable-hasheq (list (cons 'op-name* op*) ...))
             (#,(if list>=vector #'list #'vector) init* ...))))]))
