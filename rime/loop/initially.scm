#!r6rs
(library (rime loop initially)
  (export loop/core/initially)
  (import (rnrs (6))
          (rime loop keywords))
  (define (make-init-plugin s-vars s-exprs)
    (let ()
      (lambda (method . args)
        (with-syntax ([(vars ...) s-vars]
                      [(exprs ...) s-exprs])
          (case method
            [(debug)
             (object-to-string
              ":initially"
              (apply
               string-append
               (map
                (lambda (v e)
                  (object-to-string " " v " := " e))
                (map syntax->datum s-vars)
                (map syntax->datum s-exprs))))]
            [(setup)
             (map list s-vars s-exprs)]
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
             '()]
            [else (syntax-violation #'make-init-plugin "never goes here" method)])))))
  (define (loop/core/initially e)
    (let loop ([e e]
               [vars '()]
               [exprs '()])
      (syntax-case e (:initially :=)
        [(k :initially var := expr rest ...)
         (identifier? #'var)
         (loop #'(k :initially rest ...)
               (cons #'var vars)
               (cons #'expr exprs))
         ]

        [(k :initially := expr rest ...)
         (loop #'(k :initially rest ...)
               (cons (loop-return-value #'k) vars)
               (cons #'expr exprs))
         ]
        [(k :initially rest ...)
         (values (make-init-plugin (reverse vars) (reverse exprs))
                 #'(k rest ...))
         ]
        [(k rest ...)
         (values #f e)
         ]))))
