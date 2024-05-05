(library (r6rs __cond-expand cond-expand)
  (export check-library)
  (import (rnrs (6))
          (rnrs eval (6)))
  (define (check-library import-spec)
    (let ([result (guard
                      (error
                       [else #f])
                    (eval #t (environment import-spec)))])
      result)))
