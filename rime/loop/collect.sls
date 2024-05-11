#!r6rs
(library (rime loop collect)
  (export loop/core/collect)
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (rime loop plugin)
          (rime loop keywords))
  (define (make-collect-plugin s-return-value s-var s-expr append? s-cond-expr)

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

          [(loop-body)
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
            (car args))
           ]
          [else (apply default-plugin #'make-collect-plugin method args)]))))
  (define (loop/core/collect e)
    (let loop ([e e])
      (syntax-case e (:collect :append :into :if :when :unless)
        [(k :collect expr :if cond-expr :into var rest ...)
         (identifier? #'var)
         (values (make-collect-plugin (loop-return-value #'k) #'var #'expr #f #'cond-expr)
                 #'(k rest ...))
         ]
        [(k :collect expr :if cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :collect expr :if cond-expr :into return-value rest ...)))
         ]
        [(k :collect expr :when cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :collect expr :if cond-expr :into return-value rest ...)))
         ]
        [(k :collect expr :unless cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :collect expr :if (not cond-expr) :into return-value rest ...)))
         ]
        [(k :collect expr rest ...)
         (loop #'(k :collect expr :if #t rest ...))
         ]
        [(k :append expr :if cond-expr :into var rest ...)
         (identifier? #'var)
         (values (make-collect-plugin (loop-return-value #'k) #'var #'expr #t #'cond-expr)
                 #'(k rest ...))
         ]
        [(k :append expr :if cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :append expr :if cond-expr :into return-value rest ...)))
         ]
        [(k :append expr :when cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :append expr :if cond-expr :into return-value rest ...)))
         ]
        [(k :append expr :unless cond-expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :append expr :if (not cond-expr) :into return-value rest ...)))
         ]
        [(k :append expr rest ...)
         (loop #'(k :append expr :if #t rest ...))
         ]
        [(k :append expr rest ...)
         (with-syntax ([return-value (loop-return-value #'k)])
           (loop #'(k :append expr :into return-value rest ...)))
         ]
        [(k rest ...)
         (values #f e)
         ]))))
