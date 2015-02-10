#lang racket/base
;; Provides an exact count interface. As Racket also has a sampling
;; profiler, we could also provide a timing interface in the future.

(require
  racket/list
  racket/function
  racket/dict
  racket/serialize
  syntax/srcloc
  racket/contract
  "utils.rkt"
  (only-in errortrace/errortrace-lib
    profiling-enabled
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts
    make-errortrace-compile-handler))

(provide
  profile-point?
  profile-file
  make-profile-point-factory
  annotate-syn)

;; Returns two functions, a lookup function returning exact counts, and
;; function that returns relative weight (i.e. profile-query-weight)
(:: load-profile
   (-> (or/c source-location? path? path-string?)
       (values
         (-> profile-point? (or/c natural-number/c #f))
         (-> profile-point? (or/c (real-in 0 1) #f)))))
(define (load-profile stx-or-filename)
  (define snapshots
    (let ([file (profile-file stx-or-filename)])
      (with-handlers ([exn:fail:filesystem? (lambda _ '())])
        (with-input-from-file file
          (thunk
            ;; Merge all values from the same srcloc into a hashtable
            (make-hash (let ([snapshots (deserialize (read))])
                            (for/fold ([h '()])
                                      ([(k v) (in-dict snapshots)])
                              (dict-update h k (curry + v) 0)))))))))
  (define (lookup stx-or-srcloc)
    (define srcloc (build-source-location stx-or-srcloc))
    (dict-ref snapshots srcloc #f))
  (define m (apply max (cons 0 (dict-values snapshots))))
  (values
    lookup
    (lambda (stx) (cond [(lookup stx) => (λ (x) (/ x (max m 1)))] [else #f]))))

(:: load-profile-look-up
    (-> (or/c source-location? path?  path-string?)
        (-> profile-point? (or/c natural-number/c #f))))
(define (load-profile-look-up stx-or-filename)
  (let-values ([(profile-look-up _) (load-profile stx-or-filename)])
    profile-look-up))

(:: load-profile-query-weight
    (-> (or/c source-location? path? path-string?)
        (-> profile-point? (or/c (real-in 0 1) #f))))
(define (load-profile-query-weight stx-or-filename)
  (let-values ([(_ profile-query-weight) (load-profile stx-or-filename)])
    profile-query-weight))

(:: save-profile (-> (or/c source-location? path? path-string?)
                     void?))
(define (save-profile stx-or-filename)
  (with-output-to-file
    (profile-file stx-or-filename)
    (thunk (write (serialize-conv (get-execute-counts))))
    #:exists 'replace))

(define (serialize-conv v)
  (serialize (map (lambda (p) (cons (build-source-location (car p)) (cdr p))) v)))

(:: run-with-profiling (-> module-path? void?))
(define (run-with-profiling module-path)
  (parameterize* ([current-namespace (make-base-namespace)]
                  [execute-counts-enabled #t]
                  [instrumenting-enabled #t]
                  [profiling-enabled #t]
                  [current-compile (make-errortrace-compile-handler)])
    (dynamic-require module-path 0)))
