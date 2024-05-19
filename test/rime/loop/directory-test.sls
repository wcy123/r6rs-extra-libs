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
            (loop :trace-codegen
                  :name recur
                  :recur level := 0
                  :for file :in-directory "test"
                  :with is-dir := (is-directory? file)
                  :collect file :if (and (not is-dir)
                                         (string=? (substring file (fx- (string-length file) 4) (string-length file)) ".sls"))
                  :do (recur (fx+ 1 level) file) :if (and is-dir (fx<? level 2))
                  ))
           (list
            "test/hello-test.sls"
            "test/hello/world-test.sls"
            "test/rime/files/__directory-list-test.sls"
            "test/rime/logging-test.sls"
            "test/rime/loop/append-test.sls"
            "test/rime/loop/break-if-test.sls"
            "test/rime/loop/break-test.sls"
            "test/rime/loop/collect-hash-table-test.sls"
            "test/rime/loop/collect-inside-finally-test.sls"
            "test/rime/loop/count-by-test.sls"
            "test/rime/loop/count-test.sls"
            "test/rime/loop/depth-first-search-test.sls"
            "test/rime/loop/directory-test.sls"
            "test/rime/loop/finally-test.sls"
            "test/rime/loop/for-hash-table-one-two-three-test.sls"
            "test/rime/loop/for-i-in-vector-123.sls"
            "test/rime/loop/for-i-on-123-test.sls"
            "test/rime/loop/group-by-test.sls"
            "test/rime/loop/if-odd-test.sls"
            "test/rime/loop/in-directory.sls"
            "test/rime/loop/in-string-test.sls"
            "test/rime/loop/initially-test.sls"
            "test/rime/loop/join-string-test.sls"
            "test/rime/loop/list-test.sls"
            "test/rime/loop/nested-loop-test.sls"
            "test/rime/loop/plugin-test.sls"
            "test/rime/loop/recur-test.sls"
            "test/rime/loop/repeat-3-test.sls"
            "test/rime/loop/two-for-test.sls"
            "test/rime/loop/upfrom-2-no-end-test.sls"
            "test/rime/loop/with-test.sls"
            "test/rime/string/__string-find-all-test.sls"
            "test/rime/string/__string-join-test.sls"
            "test/rime/string/__string-split-test.sls"
            "test/rime/string/__string-start-with-test.sls"))))
