#lang racket/base

(provide load-profile ; for users
         save-profile) ; for profile.rkt

(require
  (only-in errortrace get-execute-counts)
  racket/list
  racket/trace
  racket/serialize
  "private/utils.rkt")

(define (serialize-conv v)
  (serialize (map (lambda (p) (cons (syntax->srcloc (car p)) (cdr p))) v)))

(define (save-profile stx-or-filename)
  (define profile-file
    (cond [(syntax? stx-or-filename)
           (source-file->profile-file (syntax-source stx-or-filename))]
          [(path-string? stx-or-filename)
           stx-or-filename]
          [else
           (error "not a filename" stx-or-filename)]))
  (with-output-to-file
    profile-file
    (lambda () (write (serialize-conv (get-execute-counts))))
    #:exists 'replace))

(define (load-profile stx-or-filename)
  (define profile-file
    (cond [(syntax? stx-or-filename)
           (source-file->profile-file (syntax-source stx-or-filename))]
          [(path-string? stx-or-filename)
           stx-or-filename]
          [else
           (error "not a filename" stx-or-filename)]))
  (define snapshots
    (with-handlers ([exn:fail:filesystem? (lambda _ '())])
      (with-input-from-file profile-file
        (lambda ()
          (deserialize (read))))))
  (lambda (stx)
    (define srcloc (syntax->srcloc stx))
    #;(define sexp   (syntax->datum stx)) ; for disambiguation
    (cond
      [(assoc srcloc snapshots) => cdr]
      [else 0])))
