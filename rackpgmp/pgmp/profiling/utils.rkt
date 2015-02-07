#lang racket/base

(require
  (only-in racket/function thunk)
  (for-syntax racket/base)
  racket/serialize
  syntax/srcloc
  racket/contract
  (only-in errortrace
    profiling-enabled
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler))

(provide ::)
(define-syntax-rule (:: id contract-expr)
  (provide id))
;(contract-out (id contract-expr))

(:: profile-point? (-> any/c boolean?))
(define profile-point? source-location?)

(provide annotate-syn)
(define-syntax (annotate-syn syn)
  (syntax-case syn ()
    [(_ profile-point syn ...)
     #'(quasisyntax/loc
         (build-source-location-syntax profile-point)
         syn ...)]))

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

(define (serialize-conv v)
  (serialize (map (lambda (p) (cons (build-source-location (car p)) (cdr p))) v)))

(:: save-profile (-> (or/c source-location? path? path-string?)
                     void?))
(define (save-profile stx-or-filename)
  (with-output-to-file
    (profile-file stx-or-filename)
    (thunk (write (serialize-conv (get-execute-counts))))
    #:exists 'replace))

(:: run-with-profiling (-> module-path? void?))
(define (run-with-profiling module-path)
  (parameterize* ([current-namespace (make-base-namespace)]
                  [execute-counts-enabled #t]
                  [instrumenting-enabled #t]
                  [profiling-enabled #t]
                  [current-compile (make-errortrace-compile-handler)])
    (dynamic-require module-path 0)))
