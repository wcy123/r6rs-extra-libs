#!r6rs
(library (rime loop list)
  (export loop/core/list)
  (import (rnrs (6))
          (rime loop keywords))

  (define (make-for-as-list s-var s-expr in/on)
    (let ([s-expr-recur-var (new-var s-var "-in-recur-list")]
          [s-expr-loop-var (new-var s-var "-in-loop-list")])
      (lambda (method . args)
        (with-syntax ([var s-var]
                      [expr-recur-var s-expr-recur-var]
                      [expr-loop-var s-expr-loop-var]
                      [expr s-expr])
          (case method
            [(debug)
             (object-to-string
              ":for " (syntax->datum #'var)
              " :in " (syntax->datum #'expr))]
            [(setup)
             (list)]
            [(recur)
             (list #'[expr-recur-var expr])]
            [(before-loop-begin)
             (list)]
            [(init)
             (list #'[expr-loop-var expr-recur-var])]
            [(loop-entry)
             (list (if (eq? in/on 'in) #'[var (car expr-loop-var)] #'[var expr-loop-var]))]
            [(continue-condition)
             #'(not (null? expr-loop-var))]
            [(loop-body)
             (let [(rest-body (car args))]
               rest-body)]
            [(step)
             (list #'(cdr expr-loop-var))]
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
