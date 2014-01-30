#lang racket/base

(provide load-profile ; for users
         save-profile) ; for run.rkt

(require
  racket/list
  racket/serialize
  "utils.rkt")

(define-syntax-rule (debugf str args ...) (void)#;(printf str args ...))

(define (serialize-conv v)
  (serialize (map (lambda (p) (cons (syntax->srcloc (car p)) (cdr p))) v)))

(define (save-profile stx-or-filename v)
  (define profile-file
    (cond [(syntax? stx-or-filename)
           (source-file->profile-file (syntax-source stx-or-filename))]
          [(path-string? stx-or-filename)
           stx-or-filename]
          [else
           (error "not a filename" stx-or-filename)]))
  (with-output-to-file
    profile-file
    (lambda () (write (serialize-conv v)))
    #:exists 'replace))

(define (load-profile stx-or-filename)
  (define profile-file
    (cond
      [(syntax? stx-or-filename)
       (source-file->profile-file (syntax-source stx-or-filename))]
      [(path-string? stx-or-filename)
       stx-or-filename]
      [else
       (error "not a filename" stx-or-filename)]))
  (define snapshots
    (with-handlers ([exn:fail:filesystem? (lambda _ '())])
      (with-input-from-file profile-file
        (lambda ()
          (debugf "Using profile info from: ~a\n" profile-file)
          (deserialize (read))))))
  (lambda (stx)
    (define srcloc (syntax->srcloc stx))
    (debugf "Looking up ~a\n" srcloc)
    #;(define sexp   (syntax->datum stx)) ; for disambiguation
    (cond
      [(assoc srcloc snapshots) => cdr]
      [else 0])))
