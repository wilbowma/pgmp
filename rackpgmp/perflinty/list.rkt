#lang racket/base
(require
  (prefix-in real: racket/list)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
    "../pgmp/api/exact.rkt"))
(provide
  list
  list?
  map
  car
  cdr
  cons
  list-ref
  length)

(struct list-rep (op-table ls))

(define-syntax (define-list-rep-op syn)
  (syntax-case syn ()
    ;; ls must appear in args ...
    [(_ name ls-rep (arg* ...))
     #`(define (name #,@(map (λ (arg)
                                (syntax-case arg (rep)
                                  [(rep a) #'a]
                                  [_ arg]))
                             (syntax->list #'(arg* ...))))
         (struct-copy list-rep ls-rep
           [ls ((hash-ref (list-rep-op-table ls-rep) 'name)
                #,@(map (λ (arg)
                           (syntax-case arg (rep)
                             [(rep a) #'(list-rep-ls a)]
                             [_ arg]))
                    (syntax->list #'(arg* ...))))]))]))

(define-list-rep-op list? ls ([rep ls]))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define-list-rep-op map ls (f [rep ls]))
(define-list-rep-op car ls ([rep ls]))
(define-list-rep-op cdr ls ([rep ls]))
;; TODO: This might need to be a macro, as it kind of needs to generate
;; new sources
(define-list-rep-op cons ls (x [rep ls]))
;; TODO: ref should probably count higher indices more expensive, but
;; that would require more clever engineering.
(define-list-rep-op list-ref ls ([rep ls] n))
(define-list-rep-op length ls ([rep ls]))

(begin-for-syntax
  (define make-profile-point (make-profile-point-factory "profiled-list")))
(define-syntax (list x)
  ;; Create fresh source object. lsrc profiles operations that are
  ;; fast on lists, and vsrc profiles operations that are fast on
  ;; vectors.
  (define profile-query-weight (load-profile-query-weight x))
  (define lsrc (make-profile-point x))
  (define vsrc (make-profile-point x))
  ;; Defines all the sequences operations, giving profiled implementations
  (define op-name* '(list? map car cdr cons list-ref length))
  (define op*
    (map
      (lambda (v src)
        (annotate-syn src (lambda args #,(annotate-syn src (apply #,v args)))))
      (syntax->list #'(real:list? real:map real:car real:cdr real:cons real:list-ref real:length))
      `(,#f        ,#f      ,lsrc      ,lsrc    ,lsrc     ,vsrc         ,vsrc)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (or (profile-query-weight lsrc) 0)
                 (or (profile-query-weight vsrc) 0))
       (printf "WARNING: You should probably reimplement this list as a vector: ~a\n" x))
     (with-syntax ([(op* ...) op*] [(op-name* ...) op-name*])
       #`(let ()
           (list-rep
             (make-immutable-hasheq (real:list (real:cons 'op-name* op*) ...))
             (real:list init* ...))))]))
