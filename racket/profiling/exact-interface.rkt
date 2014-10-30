#lang racket/base

(provide
  load-profile-info
  load-profile-query-weight
  load-profile ; for users
  save-profile) ; for run.rkt

(require
  racket/list
  racket/serialize
  "utils.rkt")

(define-syntax-rule (debugf str args ...) (void)#;(printf str args ...))

(define (serialize-conv v)
  (serialize (map (lambda (p) (cons (syntax->srcloc (car p)) (cdr p))) v)))

(define (save-profile stx-or-filename v)
  (with-output-to-file
    (profile-file stx-or-filename)
    (lambda () (write (serialize-conv v)))
    #:exists 'replace))

;; Returns two functions, a lookup function returning exact counts, and
;; function that returns relative weight (i.e. profile-query-weight)
(define (load-profile-info stx-or-filename)
  (define snapshots
    (let ([profile-file (profile-file stx-or-filename)])
      (with-handlers ([exn:fail:filesystem? (lambda _ '())])
        (with-input-from-file profile-file
          (lambda ()
            (debugf "Using profile info from: ~a\n" profile-file)
            (deserialize (read)))))))
  (define (lookup stx-or-srcloc)
    (define srcloc
      (cond
        [(syntax? stx-or-srcloc) (syntax->srcloc stx-or-srcloc)]
        [(srcloc? stx-or-srcloc) stx-or-srcloc]
        [else (error "Not syntax or srcloc" stx-or-srcloc)]))
    (debugf "Looking up ~a\n" srcloc)
    #;(define sexp   (syntax->datum stx)) ; for disambiguation
    (cond
      [(assoc srcloc snapshots) =>
       ;;cdr
       ;; somehow the generated sources end up in multiple places, so
       ;; grab them all
       (lambda (_)
         (for/sum ([p (filter (lambda (v) (equal? srcloc (car v))) snapshots)])
           (cdr p)))]
      [else 0]))
  (define m (apply max (cons 0 (map cdr snapshots))))
  (values
    lookup
    (lambda (stx) (/ (lookup stx) (max m 1)))))

(define (load-profile stx-or-filename)
  (let-values ([(look-up _) (load-profile-info stx-or-filename)])
    look-up))

(define (load-profile-query-weight stx-or-filename)
  (let-values ([(_ profile-query-weight) (load-profile-info stx-or-filename)])
    profile-query-weight))
