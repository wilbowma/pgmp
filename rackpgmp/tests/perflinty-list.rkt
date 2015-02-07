#lang racket

(require
  rackunit
  "../pgmp/perflinty/list.rkt")
(provide run)

#;(begin-for-syntax
  (define old (current-output-port))
  (define string-port (open-output-string))
  (current-output-port string-port))

#;(define-syntax (check-profiled-output-match syn)
  (syntax-case syn ()
    [(_ flag pat)
     (let ([str (datum->syntax syn (bytes->string/utf-8 (get-output-bytes string-port #t)))])
       #`(when flag (check-regexp-match pat #,str)))]))


(define (run x [profiled? #f])
  (define ls (list 1 2 3 4))

  #;(check-profiled-output-match
    profiled?
    "^WARNING: You should probably reimplement.+$")
  (unless profiled? (displayln "Expect WARNING for list '(1 2 3 4)"))

  (define ls2 (list 1 2 3 4 0))
  (unless profiled? (displayln "Expect NO WARNING for list '(1 2 3 4 0)"))
  #;(check-profiled-output-match profiled? "")
  #;(begin-for-syntax
    (current-output-port old))
  (for ([_ (in-range x)])
    (car ls)
    (list-ref ls 1)
    (list-ref ls 2)
    (list-ref ls 3)
    (list-ref ls 3)
    (car ls2)
    (car ls2)
    (list-ref ls2 3)))
