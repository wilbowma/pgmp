;; NOTE: this code doesn't run and hasn't been tested, partly due to
;; mis-matched function names, and partly because Chez is too clever and
;; throws out the reference to name in the begin statement noted below.
(define-syntax define-sequence-datatype
  (let ([ht (make-eq-hashtable)])
    (define args
      `((seq? . #'(x))
        (seq-map . #'(f s))
        (seq-first . #'(s))
        (seq-ref . #'(s n))
        (seq-set! . #'(s i obj))) )
    (define defs 
      `((make-seq . (,#'list . ,#'vector))
        (seq? . (,#'list? . ,#'vector?))
        (seq-map . (,#'map . ,#'for-each))
        (seq-first . (,#'car . ,#'(lambda (x) (vector-ref x 0))))
        (seq-ref . (,#'list-ref . ,#'vector-ref))
        (seq-set! . (,#'(lambda (ls n obj) (set-car! (list-tail ls n) obj)) . ,#'vector-set!))))
    (define (choose-args name)
      (cond 
        [(assq name defs) => cdr]
        [else (syntax-error name "not a valid sequence method:")]))
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
        [(_ var (init* ...) name* ...)
         (for-each 
           (lambda (name) (hashtable-set! ht name (or (profile-query-weight name) 0)))
           (map syntax->datum #'(name* ...)))
         (with-syntax ([(body* ...) (map (lambda (name) (choose (syntax->datum name))) #'(name* ...))]
                       [(args* ...) (map (lambda (args) (choose-args (syntax->datum name))) #'(name* ...))])
           ;; Note that a clever compiler might throw out the
           ;; reference to name inside the (begin ...) statement, thus
           ;; thwarting our attempt to use it for profiling.
           #`(begin (define (name* args* ...) (begin name* (body* args* ...))) ...
                    (define var (#,(choose 'make-seq) init* ...))))]))))

;; Examples:
;; Choose a sequence between list or vector, based on profiling info
(let ()
  ;; seq1 is defined to be either a list or vector depending on profiling
  ;; information. if seq-set! and seq-ref are used more than seq-first
  ;; and seq-map, a vector is used to implement to implement seq1.
  ;; Otherwise a list is used.
  ;;
  ;; The list of method names for seq1 are used in part to give a syntax
  ;; object to associate with each method, to profile this particular
  ;; instance of the sequence datatype. With clever annotation hackery,
  ;; it may be possible to omit them.
  (define-sequence-datatype seq1 (0 0 0 0)
    seq? 
    seq-map 
    seq-first 
    seq-ref 
    seq-set!))
