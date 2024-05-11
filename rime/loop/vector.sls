#!r6rs
(library (rime loop vector)
  (export loop/core/vector)
  (import (rnrs (6))
          (rime loop keywords))
  (define (make-vector-plugin s-var s-expr)
    (let ()
      (lambda (method . args)
        (with-syntax ([expr s-expr]
                      [var s-var]
                      [expr-var (new-var s-var "-vector-expr")]
                      [var-index (new-var s-var "-vector-index")])
          (case method
            [(debug)
             (object-to-string
              ":for " (syntax->datum #'var) " :in-vector " (syntax->datum #'expr))]
            [(setup)
             (list #'(expr-var expr))]
            [(recur)
             (list)]
            [(before-loop-begin)
             (list)]
            [(init)
             (list #'[var-index 0])]
            [(loop-entry)
             (list #'[var (vector-ref expr-var var-index)])]
            [(continue-condition)
             #'(< var-index (vector-length expr-var))]
            [(loop-body)
             (car args)]
            [(step)
             (list #'(+ 1 var-index))]
            [(finally)
             '()]
            [else (syntax-violation #'make-vector-plugin "never goes here" method)])))))

  (define (loop/core/vector e)
    (syntax-case e (:for :across :in-vector)
      [(k :for var :across expr1 rest ...)
       (values (make-vector-plugin #'var #'expr1)
               #'(k rest ...))]
      [(k :for var :in-vector expr rest ...)
       (begin
         (values (make-vector-plugin #'var #'expr)
                 #'(k rest ...)))]
      [(k rest ...)
       (values #f e)
       ])))