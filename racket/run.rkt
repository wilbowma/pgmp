#lang racket/base

(require
  (only-in errortrace
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler)
  (only-in "exact-interface.rkt"
    source-file->profile-file))

(module+ main
  (define main-module "exclusive-cond-test.rkt")

  (define profile-file (source-file->profile-file main-module))
  (parameterize ([current-namespace (make-base-namespace)]
                 [execute-counts-enabled #t]
                 [instrumenting-enabled #t])
    (delete-file profile-file)
    (displayln "EXPECTING FAILURE: ")
    (parameterize ([current-compile (make-errortrace-compile-handler)])
      ((dynamic-require main-module 'run))
      (local-require (only-in "exact-interface.rkt" save-profile))
      (save-profile profile-file)))
  (displayln "NO LONGER EXPECTING FAILURE: ")
  (parameterize ([execute-counts-enabled #f]
                 [instrumenting-enabled #f]
                 [current-namespace (make-base-namespace)])
    ((dynamic-require main-module 'run))))
