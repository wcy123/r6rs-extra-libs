#!r6rs
(library (rime loop break)
  (export loop/core/break)
  (import (rnrs (6))
          (rime loop keywords))
  (define-syntax empty-break-expr (lambda (x) (syntax-violation 'empty-break-expr "misplaced aux keyword" x)))
  (define (make-break-plugin s-return-value s-expr s-cond)
    (let ([break-flag  (car (generate-temporaries (list s-return-value)))])
      (lambda (method . args)
        (with-syntax ([expr s-expr]
                      [break-flag break-flag]
                      [:return-value s-return-value]
                      [cond s-cond])
          (case method
            [(debug)
             (object-to-string
              ":break " (syntax->datum #'expr)
              " :if " (syntax->datum #'cond))
             ]
            [(setup)
             (list)]
            [(init)
             (list #'(break-flag #f))]
            [(loop-entry)
             (list)]
            [(continue-condition)
             #'(not break-flag)]
            [(loop-body)
             (let ([rest-body (car args)])
               (with-syntax ([(rest-body ...) (car args)
                              ]
                             [(set-return-value ...)
                              (if (not
                                   (and (identifier? s-expr)
                                        (free-identifier=? s-expr
                                                           #'empty-break-expr)))
                                  (list #'(set! :return-value expr))
                                  '())])
                 (list
                  #'(begin
                      (set! break-flag cond)
                      (if break-flag
                          (begin 0 set-return-value ...)
                          (begin 0 rest-body ...))))))]
            [(step)
             (list #'break-flag)]
            [(finally)
             '()]
            [else (syntax-violation #'make-break-plugin "never goes here" method)])))))

  (define (loop/core/break e)
    (let loop ([e e])
      (syntax-case e (:break :when :if :unless empty-break-expr)
        [(k :break :when cond rest ...)
         (loop #'(k :break empty-break-expr :if cond rest ...))]
        [(k :break :unless cond rest ...)
         (loop #'(k :break empty-break-expr :if (not cond) rest ...))
         ]

        [(k :break :if cond rest ...)
         (loop #'(k :break empty-break-expr :if cond rest ...))]

        [(k :break expr :when cond rest ...)
         (loop #'(k :break expr :if cond rest ...))
         ]

        [(k :break expr :unless cond rest ...)
         (loop #'(k :break expr :if (not cond) rest ...))]
        ;; core expression
        [(k :break expr :if cond rest ...)
         (values (make-break-plugin (loop-return-value #'k)
                                    #'expr #'cond)
                 #'(k rest ...))]

        [(k :break expr rest ...)
         (not (keyword? #'expr))
         (loop #'(k :break expr :if #t rest ...))]

        [(k :break rest ...)
         (loop #'(k :break empty-break-expr :if #t rest ...))]

        [(k rest ...)
         (values #f e)
         ]))))
