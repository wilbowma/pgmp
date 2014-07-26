#lang racket/base

(require
  rackunit
  (only-in errortrace
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler)
  (only-in "../profiling/utils.rkt"
    source-file->profile-file)
  (only-in "../profiling/exact-interface.rkt" save-profile))

(define-syntax-rule (mark runs main-module run)
  (let ()
    (define profile-file (source-file->profile-file main-module))
    (instrumenting-enabled #f)

    (with-handlers ([exn:fail:filesystem? (lambda _ (void))]) (delete-file profile-file))

    ;; Run without profiling
    (time (parameterize
            ([execute-counts-enabled #f]
             [instrumenting-enabled #f]
             [current-namespace (make-base-namespace)])
            ((dynamic-require main-module run) runs)))

    ;; Profile
    (parameterize* ([current-namespace (make-base-namespace)]
                    [execute-counts-enabled #t]
                    [instrumenting-enabled #t]
                    [current-compile (make-errortrace-compile-handler)])
                   ((dynamic-require main-module run) runs))

    (save-profile profile-file (get-execute-counts))

    ;; Run with profile optimization
    (time (parameterize
            ([execute-counts-enabled #f]
             [instrumenting-enabled #f]
             [current-namespace (make-base-namespace)])
            ((dynamic-require main-module run) runs)))))

;; exclusive-cond-test.rkt
(module+ main
         (mark 10000000 "exclusive-cond-test.rkt" 'run)
         (mark 10000000 "case-test.rkt" 'run))
