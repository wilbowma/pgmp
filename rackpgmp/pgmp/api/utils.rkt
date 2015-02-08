#lang racket/base

(require
  (only-in racket/function thunk)
  (for-syntax racket/base)
  racket/serialize
  syntax/srcloc
  racket/contract)

(provide ::)
(define-syntax-rule (:: id contract-expr)
  (provide (contract-out (id contract-expr))))

(:: profile-point? (-> any/c boolean?))
(define profile-point? source-location?)

(provide annotate-syn)
(define-syntax (annotate-syn syn)
  (syntax-case syn ()
    [(_ profile-point template)
     #'(quasisyntax/loc
         (build-source-location-syntax profile-point)
         template)]))

(:: source-file->profile-file (-> (or/c path? path-string?) path-string?))
(define (source-file->profile-file f)
  (string->path
    (string-append
     (cond [(path? f) (path->string f)]
           [(string? f) f])
     ".profile")))


(:: profile-file (-> (or/c source-location? path? path-string?)
                     path-string?))
(define (profile-file stx-or-filename)
  (cond
    [(source-location? stx-or-filename)
     (source-file->profile-file (source-location-source stx-or-filename))]
    [(path? stx-or-filename)
     (path->string stx-or-filename)]
    [(path-string? stx-or-filename)
     stx-or-filename]
    [else
      (error "not a filename" stx-or-filename)]))


(:: make-profile-point-factory (-> string? (-> source-location? profile-point?)))
(define (make-profile-point-factory prefix)
  (let ([n 0])
    (lambda (syn)
      (let ([src (struct-copy srcloc (build-source-location syn)
                   [source (format "~a:~a:~a" (syntax-source syn) prefix n)])])
        (set! n (add1 n))
        src))))
