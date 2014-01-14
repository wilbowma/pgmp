#lang racket/base

(provide (all-defined-out))
(define (source-file->profile-file f)
  (string-append (cond [(path? f)   (path->string f)]
                       [(string? f) f]
                       [else        (error "not a filename" f)])
                 ".profile"))

;; TODO surely this must already exist somewhere
(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))


(define (profile-file stx-or-filename)
  (cond [(syntax? stx-or-filename)
           (source-file->profile-file (syntax-source stx-or-filename))]
          [(path-string? stx-or-filename)
           stx-or-filename]
          [else
           (error "not a filename" stx-or-filename)]))
