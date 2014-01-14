#lang racket/base

(require
  rackunit
  (only-in errortrace
    execute-counts-enabled
    instrumenting-enabled
    get-execute-counts)
  (only-in errortrace/errortrace-lib
    make-errortrace-compile-handler)
  (only-in "private/utils.rkt"
    source-file->profile-file)
  (only-in "exact-interface.rkt" save-profile))

(module+ main
  (define main-module "exclusive-cond-test.rkt")

  (define profile-file (source-file->profile-file main-module))

  (with-handlers ([exn:fail:filesystem? (lambda _ (void))]) (delete-file profile-file))

  (parameterize* ([current-namespace (make-base-namespace)]
                  [execute-counts-enabled #t]
                  [instrumenting-enabled #t]
                  [current-compile (make-errortrace-compile-handler)])
    (check-false ((dynamic-require main-module 'run))))

  (save-profile profile-file (get-execute-counts))

  (parameterize ([execute-counts-enabled #f]
                 [instrumenting-enabled #f]
                 [current-namespace (make-base-namespace)])
    (check-true ((dynamic-require main-module 'run)))))
