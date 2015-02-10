#lang racket
;; run via
;; > pwd
;; rackpgmp/pgmp/tests
;; > racket run.rkt

(require
  rackunit
  (only-in errortrace
    profiling-enabled
    profile-paths-enabled
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler)
  (only-in "../pgmp/api/exact.rkt"
    profile-file
    save-profile))

(define-syntax (maybe-time syn)
  (syntax-case syn ()
    [(_ t e)
     #`(if t
           (time e)
           e)]))
;; TODO: Capture output and check the timings are lower after second
;; run, and the result of run is true.
(define (mark runs mod-path run [time? #t])
  (let ()
    (printf "~a test:~n" mod-path)
    (instrumenting-enabled #f)

    (with-handlers ([exn:fail:filesystem? (lambda _ (void))])
                   (delete-file (profile-file mod-path)))

    (define main-module mod-path)
    (printf "  Pre-optimization:  ")
    (unless time?
      (printf "    ~n"))
    ;; Run without profiling
    (maybe-time time? (parameterize
            ([execute-counts-enabled #f]
             [instrumenting-enabled #f]
             [current-namespace (make-base-namespace)])
            ((dynamic-require main-module run) runs)))

    ;; Profile
    (parameterize* ([current-namespace (make-base-namespace)]
                    [execute-counts-enabled #t]
                    [instrumenting-enabled #t]
                    [profiling-enabled #t]
                    [profile-paths-enabled #f]
                    [current-output-port (open-output-nowhere)]
                    #;[current-error-port (open-output-nowhere)]
                    [current-compile (make-errortrace-compile-handler)])
                   ((dynamic-require main-module run) runs))

    (save-profile mod-path)

    (printf "  Post-optimization: ")
    (unless time?
      (printf "    ~n"))
    ;; Run with profile optimization
    (maybe-time time? (parameterize
            ([execute-counts-enabled #f]
             [instrumenting-enabled #f]
             [current-namespace (make-base-namespace)])
            ((dynamic-require main-module run) runs #t)))))

(module+ main
  (mark 10000000 "exclusive-cond-test.rkt" 'run)
   (newline)
  (mark 10000000 "case-test.rkt" 'run)
   (newline)
  (mark 1000000  "class-test.rkt" 'run)
   (newline)
  (mark 1000000  "perflinty-list.rkt" 'run #f)
   (newline)
  (mark 1000000  "perflinty-vector.rkt" 'run #f)
   (newline)
  (mark 8000000  "perflinty-auto.rkt" 'run))
