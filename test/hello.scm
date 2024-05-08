#!r6rs
(library (test hello)
  (export main)
  (import (rnrs (6))
          (rime unit-test __define-test)
          (rime unit-test __check))
  (define-test hello-test
    (CHECK equal? (+ 1 1) 2)
    1
    )
  (define (main)
    (display "HELLO hello main\n")
    (run-all-tests)))
