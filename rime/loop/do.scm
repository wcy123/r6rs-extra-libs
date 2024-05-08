#!r6rs
(library (rime loop do)
  (export loop/core/do)
  (import (rnrs (6))
          (rime loop keywords))
  (define (make-do-plugin s-expr)
    (let ()
      (lambda (method . args)
        (with-syntax ([expr s-expr])
          (case method
            [(debug)
             (object-to-string
              ":do " (syntax->datum #'expr))]
            [(setup)
             (list)]
            [(init)
             (list)]
            [(loop-entry)
             (list)]
            [(continue-condition)
             #t]
            [(loop-body)
             (with-syntax ([(rest-body ...) (car args)])
               (list #'(begin expr rest-body ...)))]
            [(step)
             '()]
            [(finally)
             '()]
            [else (syntax-violation #'make-do-plugin "never goes here" method)])))))
  (define (loop/core/do e)
    (syntax-case e (:do)
      [(k :do expr rest ...)
       (begin
         (values (make-do-plugin #'expr)
                 #'(k rest ...)))]
      [(k rest ...)
       (values #f e)
       ])))
