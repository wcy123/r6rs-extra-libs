#!r6rs
(library (test repl)
  (export main)
  (import (rnrs (6))
          (rime unit-test __check)
          )
  (define (main)
    (CHECK main eq? (+ 1 1) 2)
    (CHECK main eq? (+ 1 1) 3)))
