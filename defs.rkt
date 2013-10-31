#lang racket
(require scribble/base)
(require scriblib/footnote)
(provide todo)
(define (todo . x) (apply note "TODO: " x))

#;(define-code chez (curry to-paragraph #:wrap-elem smaller))
