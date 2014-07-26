#lang racket/base
(require
  (prefix-in real: racket/list)
  (prefix-in real: racket/base)
  (for-syntax
    racket/base
    "../profiling/exact-interface.rkt")
  (for-syntax (prefix-in real: racket/list)))
(provide
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
  real:length real:list real:list? real:map real:car real:cdr real:cons real:list-ref
  )

#;(module profiled-list racket/base
  )
;; TOOD: Boy does is this in need of a meta-program
;; Ought to write a macro in that generates all these, call it in a
;; submodule, export all defined in it.
(define-values
  (current-profiled-list?
   current-profiled-map
   current-profiled-car
   current-profiled-cdr
   current-profiled-cons
   current-profiled-list-ref)
  (values
    (make-parameter real:list?)
    (make-parameter real:map)
    (make-parameter real:car)
    (make-parameter real:cdr)
    (make-parameter real:cons)
    (make-parameter real:list-ref)))

;; These are delicate; profiled list must evaluated first to set parameters.
;; But Racket is CBV so it's okay for now.
(define (list? ls)
  ((current-profiled-list?) (ls)))
(define (map f . lss)
  ((current-profiled-map) f (real:map (lambda (ls) (ls)) (lss))))
(define (car ls)
  ((current-profiled-car) (ls)))
(define (cdr ls)
  ((current-profiled-cdr) (ls)))
(define (cons x ls)
  ((current-profiled-cons) x (ls)))
(define (list-ref ls n)
  ((current-profiled-list-ref) (ls) n))

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
        (let* ([src (make-srcloc (format "profiled-list~s" n)
                                 (syntax-line syn)
                                 (syntax-column syn)
                                 (syntax-position syn)
                                 (syntax-span syn))])
          (set! n (add1 n))
          src)))))

(define-syntax (list x)
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
      '(real:list? real:map real:car real:cdr real:cons real:list-ref)
      (list #f #f #f list-src list-src vector-src)))
  (syntax-case x ()
    [(_ init* ...)
     (unless (>= (look-up-profile list-src) (look-up-profile vector-src))
       (printf "WARNING: You should probably reimplement this list as a vector: ~a"
               x))
     (with-syntax ([(def* ...) op*]
                   [(name* ...) (generate-temporaries op*)]
                   [(params ...)
                    #'(current-profiled-list?
                       current-profiled-map
                       current-profiled-car
                       current-profiled-cdr
                       current-profiled-cons
                       current-profiled-list-ref)])
       #`(let ()
           (define name* def*) ...
           (lambda ()
             (params name*) ...
             (real:list init* ...))))]))
