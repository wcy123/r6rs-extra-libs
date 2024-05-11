#!r6rs
(library (rime loop if)
  (export loop/core/if)
  (import (rnrs (6))
          (rime loop keywords))
  (define (make-if-plugin s-expr)
    (let ()
      (lambda (method . args)
        (with-syntax ([expr s-expr])
          (case method
            [(debug)
             (object-to-string
              ":if " (syntax->datum #'expr))]
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
               (list #'(if expr
                           (begin
                             ;; support empty rest-body
                             0 rest-body ...))))]
            [(step)
             '()]
            [(finally)
             '()]
            [else (syntax-violation #'make-if-plugin "never goes here" method)])))))
  (define (loop/core/if e)
    (syntax-case e (:if :when :unless)
      [(k :if expr rest ...)
       (begin
         (values (make-if-plugin #'expr) #'(k rest ...)))]
      [(k :when expr rest ...)
       (begin
         (values (make-if-plugin #'expr) #'(k rest ...)))]
      [(k :unless expr rest ...)
       (begin
         (values (make-if-plugin #'(not expr)) #'(k rest ...)))]
      [(k rest ...)
       (values #f e)
       ])))
