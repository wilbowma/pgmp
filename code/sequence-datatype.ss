(define-syntax define-sequence-datatype
  (let ([ht (make-eq-hashtable)])
    (define defs 
      `((make-seq . (,#'list . ,#'vector))
        (seq? . (,#'list? . ,#'vector?))
        (seq-map . (,#'map . ,#'for-each))
        (seq-first . (,#'car . ,#'(lambda (x) (vector-ref x 0))))
        (seq-ref . (,#'list-ref . ,#'vector-ref))
        (seq-set! . (,#'(lambda (ls n obj) (set-car! (list-tail ls n) obj)) . ,#'vector-set!))))
    (define (default-def name)
      (car (cdr (assq name defs))))
    (define (choose name)
      (let ([seq-set!-count (hashtable-ref ht 'seq-set! 0)]
            [seq-ref-count (hashtable-ref ht 'seq-ref 0)]
            [seq-first-count (hashtable-ref ht 'seq-first 0)]
            [seq-map-count (hashtable-ref ht 'seq-map 0)])
      (cond 
        [(assq name defs) => 
          (lambda (x)
            (let ([x (cdr x)])
              (if (> (+ seq-set!-count seq-ref-count) (+ seq-first-count seq-map-count))
                  (cdr x)
                  (car x))))]
        [else (syntax-error name "not a valid sequence method:")])))
    (lambda (x)
      (syntax-case x ()
        [(_ (name* args* ...) ...)
         (for-each 
           (lambda (name) (hashtable-set! ht name (or (profile-query-count-syntax (default-def name)) 0)))
           (map syntax->datum #'(name* ...)))
         (with-syntax ([(body* ...) (map (lambda (name) (choose (syntax->datum name))) #'(name* ...))])
           #'(begin (define (name* args* ...) (body* args* ...)) ...))]))))
;; Choose a sequence between list or vector, based on profiling info
(define-sequence-datatype  
  (make-seq x y z)
  (seq? s)
  (seq-map f s)
  (seq-first s)
  (seq-ref s n)
  (seq-set! s n obj))
