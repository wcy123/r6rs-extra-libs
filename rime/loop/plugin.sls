#!r6rs
(library (rime loop plugin)
  (export default-plugin)
  (import (rnrs (6)))
  (define (default-plugin who method . args)
    (case method
      [(debug)
       "<NOT DEFINED"]
      [(setup)
       (list)]
      [(recur)
       (list)]
      [(before-loop-begin)
       (list)]
      [(init)
       (list)]
      [(loop-entry)
       (list)]
      [(continue-condition)
       #t]
      [(loop-body)
       (car args)]
      [(step)
       (list)]
      [(finally)
       (list)]
      [(is-finally?)
       #f]
      [else (syntax-violation who "never goes here" method)])))
