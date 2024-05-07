#!r6rs
(library (rime rime-0 __cond-expand cond-expand)
  (export check-library check-feature)
  (import (rnrs (6))
          (rnrs eval (6)))

  (define-syntax define-feature-if-import-set
    (syntax-rules ()
      [(_ feature-id import-spec)
       (cons (quote feature-id)
             (guard
                 (exn
                  [else #f])
               (eval #t (environment (quote import-spec)))))]))

  (define feature-registry
    (list
     (define-feature-if-import-set guile (guile))
     (define-feature-if-import-set chezscheme (chezscheme))
     ))

  (define (check-feature feature-id)
    (assq feature-id feature-registry))

  (define (check-library import-spec)
    (let ([result (guard
                      (error
                       [else #f])
                    (eval #t (environment import-spec)))])
      result)))
