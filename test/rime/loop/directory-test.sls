#!r6rs
(library (test rime loop directory-test)
  (export main)
  (import (rnrs (6))
          (rime unit-test)
          (rime files __is-directory)
          (rime loop)
          (rime logging)
          )
  (define (main)
    (run-all-tests))
  (define-test
    test-basic-directory
    (CHECK equal?
           (list-sort
            string<=?
            (loop :for file :in-directory "test"
                  :collect file))
           (list "test/google" "test/hello" "test/hello-test.sls" "test/rime")))
  (define-test
    test-walk-directory
    (CHECK equal?
           (list-sort
            string<=?
            (loop :name recur
                  :recur level := 0
                  :for file :in-directory "test"
                  :with is-dir := (is-directory? file)
                  :collect file :if (and (not is-dir)
                                         (string=? (substring file (fx- (string-length file) 4) (string-length file)) ".sls"))
                  :do (recur (fx+ 1 level) file) :if (and is-dir (fx<? level 1))
                  ))
           (list
            "test/hello-test.sls" "test/hello/world-test.sls" "test/rime/logging-test.sls"))))
