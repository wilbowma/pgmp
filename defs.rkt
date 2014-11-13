#lang racket
(require scribble/base)
(provide todo)
(define (todo . x) "" #;(apply margin-note "TODO: " x))

#;(define-code chez (curry to-paragraph #:wrap-elem smaller))
