#!r6rs
(library (rime loop collect)
  (export loop/core/collect)
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (rime loop keywords))
  (define (make-collect-plugin s-return-value s-var s-expr append?)

    (lambda (method . args)
      (with-syntax ([var s-var]
                    [return-value s-return-value]
                    [var-tail (new-var s-var "-tail")]
                    [expr s-expr])
        (define (gen-collect-body a-expr)
          (with-syntax ([expr a-expr])
            #'(if (null? var)
                  (set! var (cons expr '()))
                  (begin
                    (when (null? var-tail)
                      (let find-tail ([var var])
                        (if (not (null? (cdr var)))
                            (find-tail (cdr var))
                            (set! var-tail var))))
                    (set-cdr! var-tail (cons expr '()))
                    (set! var-tail (cdr var-tail))))))
        (case method
          [(debug)
           (object-to-string
            (if append? ":append" ":collect") " "
            (syntax->datum s-expr) " :into " (syntax->datum s-var))
           ]
          [(setup)
           (list
            #'(var '() #t)
            #'(var-tail '()))]
          [(init)
           (list)]
          [(loop-entry)
           (list)]
          [(continue-condition)
           #t]
          [(loop-body)
           (cons
            (if (not append?)
                (gen-collect-body #'expr)
                (let ([s-tmp (car (generate-temporaries (list #'var)))])
                  (with-syntax ([inner-body (gen-collect-body s-tmp)]
                                [tmp s-tmp])
                    #'(let local-loop ([e expr])
                        (if (not (null? e))
                            (let ([tmp (car e)])
                              inner-body
                              (local-loop (cdr e))))))))
            (car args))
           ]
          [(step)
           '()]
          [(finally)
           '()]
          [else (syntax-violation #'make-break-plugin "never goes here" method)]))))
  (define (loop/core/collect e)
    (let loop ([e e])
      (syntax-case e (:collect :append :into)
        [(k :collect expr :into var rest ...)
         (identifier? #'var)
         (values (make-collect-plugin (loop-return-value #'k) #'var #'expr #f)
                 #'(k rest ...))
         ]
        [(k :collect expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :collect expr :into return-value rest ...)))
         ]

        [(k :append expr :into var rest ...)
         (identifier? #'var)
         (values (make-collect-plugin (loop-return-value #'k) #'var #'expr #t)
                 #'(k rest ...))
         ]

        [(k :append expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :append expr :into return-value rest ...)))
         ]
        [(k rest ...)
         (values #f e)
         ]))))
