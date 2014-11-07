(library (perflinty auto)
 (export
   seq?
   seq-map
   seq-first
   seq-rest
   seq-cons
   seq-append
   seq-copy
   seq-ref
   seq-set!
   seq
   seq-length)
 (import
   (chezscheme)
   (utils))

 (define first car)
 (define rest cdr)
 (define (list-set! ls i v)
   (set! ls
     (let loop ([i i] [ls ls])
       (if (zero? i)
           (cons v (cdr ls))
           (cons (car ls) (loop (sub1 i) (cdr ls)))))))

 (define (vector-first v) (vector-ref v 1))
 (define (vector-rest v) (list->vector (cdr (vector->list v))))
 (define (vector-append vec1 vec2)
   (list->vector (append (vector->list vec1) (vector->list vec2))))
 (define (vector-cons v vec)
   (vector-append (vector v) vec))

 (define-record seq-rep (op-table s))

 (define-syntax (define-seq-rep-op syn)
   (syntax-case syn ()
     ;; s must appear in args ...
     [(_ name seq (arg* ...))
      #`(define (name #,@(map (lambda (arg)
                                (syntax-case arg (rep)
                                  [(rep a) #'a]
                                  [_ arg]))
                           (syntax->list #'(arg* ...))))
          (make-seq-rep (seq-rep-op-table seq)
            ((hashtable-ref (seq-rep-op-table seq) 'name #f)
             #,@(map (lambda (arg)
                       (syntax-case arg (rep)
                         [(rep a) #'(seq-rep-s a)]
                         [_ arg]))
                  (syntax->list #'(arg* ...))))))]))

 (define-seq-rep-op seq? s ([rep s]))
 (define-seq-rep-op seq-map s (f [rep s]))
 (define-seq-rep-op seq-first s ([rep s]))
 (define-seq-rep-op seq-rest s ([rep s]))
 (define-seq-rep-op seq-cons s (v [rep s]))
 ;; TODO: This might need to be a macro, as it kind of needs to generate
 ;; new sources
 (define-seq-rep-op seq-append s2 ([rep s1] [rep s2]))
 (define-seq-rep-op seq-copy s ([rep s]))
 (define-seq-rep-op seq-ref s ([rep s] p))
 (define-seq-rep-op seq-set! s ([rep s] p v))
 (define-seq-rep-op seq-length s ([rep s]))

 (meta define make-fresh-source-obj! (make-fresh-source-obj-factory! "profiled-sequence"))
 (define-syntax (seq x)
   ;; Create fresh source object. list-src profiles operations that are
   ;; fast on lists, and vector-src profiles operations that are fast on
   ;; vectors.
   (define list-src (make-fresh-source-obj! x))
   (define vector-src (make-fresh-source-obj! x))
   (define previous-list-usage (profile-query-weight list-src))
   (define previous-vector-usage (profile-query-weight vector-src))
   (define list>=vector (>= previous-list-usage previous-vector-usage))
   ;; Defines all the sequences operations, giving profiled implementations
   (define op-name* '(seq? seq-map seq-first seq-rest seq-cons seq-append
                           seq-copy seq-ref seq-set! seq-length))
   (define op*
     (map
       (lambda (v src)
         (datum->annotated-syntax x `(lambda args (apply ,v args)) src))
       (if list>=vector
           '(list? map first rest cons append list-copy list-ref
             list-set! length)
           '(vector? vector-map vector-first vector-rest vector-cons
             vector-append vector-copy vector-ref vector-set!
             vector-length))
       (list #f #f #f list-src list-src list-src #f vector-src vector-src
             vector-src)))
   (syntax-case x ()
     [(_ init* ...)
      #`(let ()
          (make-seq-rep
            (let ([ht (make-eq-hashtable)])
                #,@(map (lambda (op op-name) #`(hashtable-set! ht #,op-name #,op))
                     (syntax->list op*) (syntax->list op-name*))
                ht)
            (#,(if list>=vector #'list #'vector) init* ...)))])))
