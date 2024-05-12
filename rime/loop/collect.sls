#!r6rs
(library (rime loop collect)
  (export loop/core/collect)
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (rime loop plugin)
          (rime loop keywords))
  (define (make-collect-plugin s-var s-expr append? s-cond-expr)
    (lambda (method . args)
      (with-syntax ([var s-var]
                    [var-tail (new-sym s-var "collect:tail")]
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

          [(loop-body finally)
           (cons
            (with-syntax ([cond-expr s-cond-expr])
              (if (not append?)
                  #`(when cond-expr
                      #,(gen-collect-body #'expr))
                  (let ([s-tmp (car (generate-temporaries (list #'var)))])
                    (with-syntax ([inner-body (gen-collect-body s-tmp)]
                                  [tmp s-tmp])
                      #'(when cond-expr
                          (let local-loop ([e expr])
                            (if (not (null? e))
                                (let ([tmp (car e)])
                                  inner-body
                                  (local-loop (cdr e))))))))))
            (if (null? args) '() (car args)))
           ]
          [else (apply default-plugin #'make-collect-plugin method args)]))))
  (define (loop/core/collect original-e)
    (let loop ([e original-e])
      (syntax-case e (:collect :append :into :if :when :unless)
        [(k :collect expr rest ...)
         (loop #'(k (:collect #f) expr rest ...))
         ]

        [(k :append expr rest ...)
         (loop #'(k (:collect #t) expr rest ...))
         ]

        [(k (:collect append?) expr rest ...)
         (loop #'(k (:collect append? expr) rest ...))
         ]

        [(k (:collect append? expr) :if cond-expr rest ...)
         (loop #'(k (:collect append? expr :if cond-expr) rest ...))
         ]

        [(k (:collect append? expr) :when cond-expr rest ...)
         (loop #'(k (:collect append? expr :if cond-expr) rest ...))
         ]

        [(k (:collect append? expr) :unless cond-expr rest ...)
         (loop #'(k (:collect append? expr :if (not cond-expr)) rest ...))
         ]

        [(k (:collect append? expr) rest ...)
         (loop #'(k (:collect append? expr :if #t) rest ...))
         ]

        [(k (:collect append? expr :if cond-expr) :into var rest ...)
         (loop #'(k (:collect append? expr :if cond-expr :into var) rest ...))
         ]

        [(k (:collect append? expr :if cond-expr) rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k (:collect append? expr :if cond-expr :into return-value) rest ...)))
         ]

        [(k (:collect append? expr :if cond-expr :into var) rest ...)
         (values (make-collect-plugin
                  #'var #'expr
                  (syntax->datum #'append?)
                  #'cond-expr)
                 #'(k rest ...))
         ]

        [(k rest ...)
         (values #f e)
         ]
        ))))
