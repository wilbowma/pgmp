#lang racket/base

(require
  racket/list
  racket/function
  racket/dict
  racket/serialize
  "utils.rkt")

(provide
  syntax->srcloc
  srcloc->list
  make-fresh-source-obj-factory!
  load-profile-info
  load-profile-query-weight
  load-profile)

;; Returns two functions, a lookup function returning exact counts, and
;; function that returns relative weight (i.e. profile-query-weight)
(define (load-profile-info stx-or-filename)
  (define snapshots
    (let ([profile-file (profile-file stx-or-filename)])
      (with-handlers ([exn:fail:filesystem? (lambda _ '())])
        (with-input-from-file profile-file
          (lambda ()
            (debugf "Using profile info from: ~a\n" profile-file)
            ;; Merge all values from the same srcloc
            (make-hash (let ([snapshots (deserialize (read))])
                  (for/fold ([h '()])
                            ([(k v) (in-dict snapshots)])
                            (dict-update h k (curry + v) 0)))))))))
  (define (lookup stx-or-srcloc)
    (define srcloc
      (cond
        [(syntax? stx-or-srcloc) (syntax->srcloc stx-or-srcloc)]
        [(srcloc? stx-or-srcloc) stx-or-srcloc]
        [else (error "Not syntax or srcloc" stx-or-srcloc)]))
    (debugf "Looking up ~a\n" srcloc)
    #;(define sexp   (syntax->datum stx)) ; for disambiguation
    (dict-ref snapshots srcloc #f))
  (define m (apply max (cons 0 (dict-values snapshots))))
  (values
    lookup
    (lambda (stx) (cond [(lookup stx) => (λ (x) (/ x (max m 1)))] [else #f]))))

(define (load-profile stx-or-filename)
  (let-values ([(look-up _) (load-profile-info stx-or-filename)])
    look-up))

(define (load-profile-query-weight stx-or-filename)
  (let-values ([(_ profile-query-weight) (load-profile-info stx-or-filename)])
    profile-query-weight))
