#!r6rs
(library (test rime files __directory-list-test)
  (export main)
  (import (rnrs (6))
          (rime files __directory-list)
          (rime unit-test))

  (define (main)
    (run-all-tests))
  (define-test hello-test
    (CHECK equal? (list-sort string<=? (directory-list "test"))
           (list  "test/google" "test/hello" "test/hello-test.sls" "test/rime"))))
