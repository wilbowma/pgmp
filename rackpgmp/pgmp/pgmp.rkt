#lang racket/base

(require
  racket/cmdline
  (only-in "api/exact.rkt"
    save-profile
    run-with-profiling))

(module+ main
  (define module-path (make-parameter #f))
  (command-line
    #:program "pgmp"
    #:once-any
    [("--profile")
     path
     "Runs the module's main submodule with profiling enabled, and dumps the profile information"
     (module-path path)]
    #:args ()
     (when (module-path)
       (run-with-profiling `(submod ,(module-path) main))
       (save-profile (module-path)))))
