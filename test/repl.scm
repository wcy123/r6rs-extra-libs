#!r6rs
(library (test repl)
  (export main)
  (import (rnrs (6))
          (rime logging)
          (rime unit-test)
          )
  (define-test test:hello-world-1
    (CHECK main eq? (+ 1 1) 2)
    (logger :debug "HELLO WORLD")
    )

  (define-test test:hello-world-2
    (CHECK main eq? (+ 1 1) 2)
    (logger :debug "HELLO WORLD")
    )

  (define (main)
    (log-level 'info)
    (run-all-tests)
    ))
