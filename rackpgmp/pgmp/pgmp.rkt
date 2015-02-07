#lang racket/base

(require
  racket/cmdline
  (only-in errortrace
    profiling-enabled
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler)
  (only-in "profiling/utils.rkt"
    source-file->profile-file
    save-profile))

(module+ main
  (define module-path (make-parameter #f))
  (define tests? (make-parameter #f))
  (command-line
    #:program "pgmp"
    #:once-any
    [("--profile")
     path
     "Runs the module with profiling enabled, and dumps the profile information"
     (module-path path)]
    #:args ()
     (when (module-path)
       (parameterize* ([current-namespace (make-base-namespace)]
                       [execute-counts-enabled #t]
                       [instrumenting-enabled #t]
                       [profiling-enabled #t]
                       [current-compile (make-errortrace-compile-handler)])
         (dynamic-require `(submod ,(module-path) main) 0))
       (save-profile (source-file->profile-file (module-path)) (get-execute-counts)))))
