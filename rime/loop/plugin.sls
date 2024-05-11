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
       (with-syntax ([(rest-body ...) (car args)])
         (list #'(begin
                   rest-body ...)))]
      [(step)
       (list)]
      [(finally)
       (list)]
      [else (syntax-violation who "never goes here" method)])))
