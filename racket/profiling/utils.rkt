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

(define (srcloc->list srcloc)
  (and srcloc
       (list (srcloc-source srcloc)
             (srcloc-line srcloc)
             (srcloc-column srcloc)
             (srcloc-position srcloc)
             (srcloc-span srcloc))))

(define (make-fresh-source-obj-factory! prefix)
  (let ([n 0])
    (lambda (syn)
      (let ([src (make-srcloc
                   (format "~a:~a:~a"
                           (syntax-source syn) prefix n) (syntax-line syn)
                   (syntax-column syn)
                   (syntax-position syn)
                   (syntax-span syn))])
        (set! n (add1 n))
        src))))
