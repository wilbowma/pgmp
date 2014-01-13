#lang racket
(require 
  "profile.rkt"
  (for-syntax "macro-interface.rkt"))

(define-syntax (define-sequence-datatype x)
  ;; Create fresh source object. list-src profiles operations that are
  ;; fast on lists, and vector-src profiles operations that are fast on
  ;; vectors.
  (define list-src (make-fresh-source-obj!))
  (define vector-src (make-fresh-source-obj!))
  ;; Defines all the sequences operations, giving implementations for
  ;; lists and vectors.
  (define op*
    `((make-seq ,#'list ,#'vector)
      (seq? ,#'list? ,#'vector?)
      (seq-map ,#'map ,#'vector-map)
      (seq-first ,#'first ,#'(lambda (x) (vector-ref x 0)))
      ;; Wrap the operations we care about with a profile form
      (seq-rest ,#`(lambda (ls) (profile #,list-src) (rest ls))
                ,#`(lambda (v)
                     (profile #,list-src)
                     (let ([i 1]
                           [v-new (make-vector (sub1 (vector-length v)))])
                       (vector-for-each
                         (lambda (x)
                           (vector-set! v-new i x)
                           (set! i (add1 i)))
                         v))))
      (seq-cons ,#`(lambda (x ls) (profile #,list-src) (cons x ls))
                ,#`(lambda (x v)
                     (profile #,list-src)
                     (let ([i 0]
                           [v-new (make-vector (add1 (vector-length v)))])
                       (vector-for-each
                         (lambda (x)
                           (vector-set! v-new i x)
                           (set! i (add1 i)))
                         v))))
      (seq-ref ,#`(lambda (ls n) (profile #,vector-src) (list-ref ls n))
               ,#`(lambda (v n) (profile #,vector-src (vector-ref v n))))
      (seq-set! ,#`(lambda (ls n obj)
                     (profile #,vector-src) (set-car! (list-tail ls n) obj)
                ,#`(lambda (v n obj)
                     (profile #,vector-src) (vector-set! v n obj))))))
    ;; Default to list; switch to vector when profile information
    ;; suggests we should.
    (define (choose-op name)
      ((if (> (profile-query-weight vector-src)
              (profile-query-weight list-src))
          third
          second)
       (assq name op*)))
  (syntax-case x ()
    [(_ var (init* ...))
     ;; Create lists of syntax for operation names and definitions
     (with-syntax ([(name* ...) (map first op*)]
                   [(def* ...) (map choose (map first op*))])
       ;; and generate them
       #`(begin (define name* def*) ...
       ;; Finally, bind the sequence.
                (define var (#,(choose 'make-seq) init* ...))))]))
