#!r6rs
(library (test rime loop in-string-test)
  (export main)
  (import (rnrs (6))
          (rime unit-test)
          (rime loop)
          )
  (define (main)
    (run-all-tests))
  (define-test
    test-in-string
    (CHECK equal? (loop :for ch :in-string "HELLO"
                        :collect ch)
           (string->list "HELLO"))
    (CHECK equal? (loop :for ch :in-string "HELLO" :offset 1
                        :collect ch)
           (string->list "ELLO"))
    ))
