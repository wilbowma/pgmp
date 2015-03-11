#lang racket
(require scribble/base scribble/core)
(provide todo titlenote)
(define (todo . x) (apply margin-note "TODO: " x))

#;(define-code chez (curry to-paragraph #:wrap-elem smaller))
(define (titlenote . items) (make-element (make-style "titlenote" '(exact-chars)) items))
