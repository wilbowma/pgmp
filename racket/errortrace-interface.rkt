#lang racket/base

(require
  (only-in errortrace get-execute-counts)
  (only-in racket/pretty pretty-write)
  racket/trace)

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))
(provide
  profile-load-data
  profile-dump-data
  profile-query-weight)

(define profile-data (make-parameter '()))

(define (profile-load-data x)
  (profile-data (with-input-from-file x read)))

(define (profile-dump-data x)
  (with-output-to-file x (lambda () (pretty-write (get-execute-counts)))
                       #:exists 'replace))

(trace-define (profile-query-weight stx-or-srcloc)
  (cond
    [(srcloc? stx-or-srcloc)
     (or (assoc stx-or-srcloc (filter syntax->srcloc (get-execute-counts))) 0)]
    [(syntax? stx-or-srcloc) (profile-query-weight (syntax->srcloc stx-or-srcloc))]
    [else (error 'profile-query-weight "Bad things: ~a\n" stx-or-srcloc)]))


