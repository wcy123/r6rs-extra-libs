#!r6rs
(library (rime os environ)
  (export getenv)
  (import (rnrs (6))
          (for (rime rime-0 cond-expand) expand))
  (cond-expand
   [(library (guile))
    (use (guile) getenv)]
   [(library (chezscheme))
    (use (chezscheme) getenv)]
   [else (define (getenv var)
           (raise-continuable
            (condition
             (make-warning)
             (make-message-condition
              "getenv is not implemented")))
           #f)]))
