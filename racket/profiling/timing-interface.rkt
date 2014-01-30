#lang racket/base

(provide source-file->profile-file ; for profile.rkt
         load-profile) ; for users

(require profile/analyzer
         racket/list racket/serialize
         "utils.rkt")

;; Can take a syntax object, in which case it looks up the profile file
;; based on its source location info, or a path string or a path pointing
;; to the profile file directly.
;; Returns a function mapping syntax objects to total time and self time.
(define (load-profile stx-or-filename)
  (define profile-file
    (cond [(syntax? stx-or-filename)
           (source-file->profile-file (syntax-source stx-or-filename))]
          [(path-string? stx-or-filename)
           stx-or-filename]
          [else
           (error "not a filename" stx-or-filename)]))
  (define snapshots
    (with-handlers ([exn:fail:filesystem?
                     ;; If file not found. Total time 0, no samples.
                     ;; Using the profiler's undocumented data format.
                     ;; May break if the format changes (unlikely).
                     (lambda _ '(0))])
      (with-input-from-file profile-file
        (lambda ()
          (deserialize (read))))))
  (define profile (analyze-samples snapshots))
  (define nodes   (profile-nodes profile))
  (define (return-node n)
    (values (node-total n)
            (node-self n)))

  (lambda (stx)
    (define srcloc (syntax->srcloc stx))
    (define sexp   (syntax->datum stx)) ; for disambiguation
    (define matching-nodes
      ;; If multiple pieces of syntax have the same source location,
      ;; there may be multiple matches.
      (for/list ([n (in-list nodes)]
                 #:when (equal? srcloc (node-src n)))
        n))
    (cond [(empty? matching-nodes)
           ;; Found nothing.
           ;; Usually means that the code was either never run, or was
           ;; never samples (so didn't take much time).
           (values #f #f)] ; Return right number of values.
          [(= (length matching-nodes) 1)
           ;; We're done.
           (return-node (first matching-nodes))]
          ;; Disambiguate based on s-exp.
          ;; Only works if `stx' is source syntax, since errortrace gives us
          ;; recovered source syntax.
          ;; Pick the first that matches.
          [(for/first ([n (in-list matching-nodes)]
                       #:when (equal? (node-id n) sexp))
             n) => return-node]
          [else
           ;; Eh, let's just pick the first one
           (return-node (first matching-nodes))])))
