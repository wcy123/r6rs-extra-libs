#!r6rs
(library (rime loop finally)
  (export loop/core/finally)
  (import (rnrs (6))
          (rime loop plugin)
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
            [(step)
             '()]
            [(finally)
             (list #'(set! :return-value expr))]
            [(is-finally?) #t]
            [else (apply default-plugin #'make-finally-plugin method args)])))))
  (define (loop/core/finally original-e)
    (let loop ([e original-e])
      (syntax-case e (:finally)

        [(k :finally expr rest ...)
         (not (keyword? #'expr))
         (begin
           (values (make-finally-plugin #'expr (loop-return-value #'k))
                   #'(k rest ...)))]

        [(k :finally expr rest ...)
         (keyword? #'expr)
         (with-syntax ([ret (loop-return-value #'k)])
           (loop #'(k :finally ret expr rest ...)))
         ]

        [(k rest ...)
         (values #f e)
         ]))))
