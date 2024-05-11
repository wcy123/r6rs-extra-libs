#!r6rs
(library (rime loop finally)
  (export loop/core/finally)
  (import (rnrs (6))
          (rime loop keywords))
  (define (make-finally-plugin s-expr s-return-value)
    (let ()
      (lambda (method . args)
        (with-syntax ([expr s-expr]
                      [:return-value s-return-value])
          (case method
            [(debug)
             (object-to-string
              ":finally " (syntax->datum #'expr))]
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
             '()]
            [(finally)
             (list #'(set! :return-value expr))]
            [else (syntax-violation #'make-finally-plugin "never goes here" method)])))))
  (define (loop/core/finally e)
    (syntax-case e (:finally)
      [(k :finally expr rest ...)
       (begin
         (values (make-finally-plugin #'expr (loop-return-value #'k))
                 #'(k rest ...)))]
      [(k rest ...)
       (values #f e)
       ])))
