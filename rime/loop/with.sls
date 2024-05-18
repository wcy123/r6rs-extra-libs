#!r6rs
(library (rime loop with)
  (export loop/core/with)
  (import (rnrs (6))
          (rime loop plugin)
          (rime loop keywords))
  (define (make-with-plugin s-var s-expr s-init-value weak)
    (let ()
      (lambda (method . args)
        (with-syntax ([var s-var]
                      [expr s-expr]
                      [init s-init-value])
          (case method
            [(debug)
             (object-to-string
              ":with " (syntax->datum #'var)
              " := " (syntax->datum #'expr)
              " :initially " (syntax->datum #'init))]
            [(setup)
             (list #'(var init weak))]
            [(iteration-body)
             (with-syntax ([(rest-body ...) (car args)])
               (list #'(begin
                         [set! var expr]
                         rest-body ...)))]
            [else (apply default-plugin #'make-with-plugin method args)])))))
  (define (loop/core/with e)
    (let repeat ([e e]
                 [init #'(if #f 0)]
                 [weak #t])
      (syntax-case e (:with := :into)
        [(k :with var := expr rest ...)
         (identifier? #'var)
         (begin
           (values (make-with-plugin #'var #'expr init weak)
                   #'(k rest ...)))]
        [(k rest ...)
         (values #f e)
         ]))))
