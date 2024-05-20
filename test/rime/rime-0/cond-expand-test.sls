#!r6rs
(library (test rime rime-0 cond-expand-test)
  (export main)
  (import (rnrs (6))
          (rime unit-test)
          (rime rime-0)
          (rime logging)
          )
  (define (main)
    (run-all-tests))
  (define-test
    test-print-darwin-windows-linux
    (cond-expand
     [darwin
      (logger :info "hello darwin")]
     [windows
      (logger :info "hello windows")]
     [linux
      (logger :info "hello linux")]
     [else
      (logger :info "hello unknown system")]
     ))

  (define-test
    test-print-imp
    (cond-expand
     [guile
      (logger :info "hello guile")]
     [chezscheme
      (logger :info "hello chezscheme")]
     [else
      (logger :info "hello unknown implementation")]
     )))
