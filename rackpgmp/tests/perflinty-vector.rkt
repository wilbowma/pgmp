#lang racket

(require
  "../perflinty/vector.rkt"
  rackunit)
(provide run)


#;(begin-for-syntax
  (define old (current-output-port))
  (define string-port (open-output-string))
  (current-output-port string-port))

#;(define-syntax (check-profiled-output-match syn)
    (syntax-case syn ()
      [(_ pat)
       (let ([str (datum->syntax syn (bytes->string/utf-8
                                       (get-output-bytes string-port #t)))])
         #`(check-profiled-regexp-match pat #,str))]))
#;(begin-for-syntax
  (current-output-port old))

(define (run x [profiled? #f])
  (define ls (vector 1 2 3 4))
  (unless profiled? (displayln "Expect NO WARNING for vector #(1 2 3 4)"))
  #;(check-profiled-output-match
    "^WARNING: You should probably reimplement.+$")
  ;; TODO: vector-append doesn't yet generate a new source
  #;(define ls2 (vector-append (vector 1) ls))
  #;(check-profiled-output-match "")
  #;(define ls3 (vector-append (vector 2) ls2))
  #;(check-profiled-output-match
    "^WARNING: You should probably reimplement.+$")
  (define ls4 (vector->list (vector 1 2 3 4 0)))
  (unless profiled? (displayln "Expect WARNING for vector #(1 2 3 4 0)"))

  (for ([_ (in-range x)])
    (vector-set! ls 2 2)))
