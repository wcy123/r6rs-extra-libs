#!r6rs
(library (rime loop list)
  (export loop/core/list)
  (import (rnrs (6))
          (rime loop keywords))

  (define (make-for-as-list s-var s-expr in/on)
    (let ([s-expr-var (new-var s-var "-in-list")])

      #!r6rs
      (lambda (method . args)
        (with-syntax ([var s-var]
                      [expr-var s-expr-var]
                      [expr s-expr])
          (case method
            [(debug)
             (object-to-string
              ":for " (syntax->datum #'var)
              " :in " (syntax->datum #'expr))]
            [(setup)
             (list)]
            [(init)
             (list #'[expr-var expr])]
            [(loop-entry)
             (list (if (eq? in/on 'in) #'[var (car expr-var)] #'[var expr-var]))]
            [(continue-condition)
             #'(not (null? expr-var))]
            [(loop-body)
             (let [(rest-body (car args))]
               rest-body)]
            [(step)
             (list #'(cdr expr-var))]
            [(finally)
             '()]
            [else (syntax-violation #'make-for-as-list "never goes here" method)])))))
  (define (loop/core/list e)
    (syntax-case e (:for :in :on)
      [(k :for i :in expr1 rest ...)
       (begin
         (values (make-for-as-list #'i #'expr1 'in)
                 #'(k rest ...)))]
      [(k :for i :on expr1 rest ...)
       (begin
         (values (make-for-as-list #'i #'expr1 'on)
                 #'(k rest ...)))]
      [(k rest ...)
       (values #f e)
       ])))
