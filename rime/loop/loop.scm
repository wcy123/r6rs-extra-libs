#!r6rs
(library (rime loop loop)
  (export loop/core/loop)
  (import (rnrs (6))
          (rime loop keywords)
          (rime loop core))

  (define (display-loop-plugin level s-loop-expr clauses)
    (object-to-string
     ":loop@" level " "
     (syntax->datum s-loop-expr)
     (loop-clauses-to-string (fx+ level 1) clauses)))

  (define (make-loop-plugin s-k s-loop-expr s-recur-name)
    (let-values ([(clauses s-next-k) (parse-loop-clauses s-loop-expr)])
      (let ([loop-level (loop-level s-k)])
        (lambda (method . args)
          (with-syntax ()
            (case method
              [(debug)
               (display-loop-plugin loop-level s-loop-expr clauses)]
              [(setup)
               (apply append (map (lambda (c) (c 'setup)) clauses))]
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
               (cons
                (with-syntax
                    ([([recur-binding-vars recur-binding-values] ...)
                      (apply append (map (lambda (f) (f 'recur)) clauses))]
                     [([before-loop-begin-vars before-loop-begin-values] ...)
                      (apply append (map (lambda (f) (f 'before-loop-begin)) clauses))]
                     [([binding-vars binding-values] ...)
                      (apply append (map (lambda (f) (f 'init)) clauses))]
                     [bindings-on-loop-begin
                      (apply append (map (lambda (f) (f 'loop-entry)) clauses))]
                     [(continue-condition ...)
                      (remove #t (map (lambda (f) (f 'continue-condition)) clauses))]
                     [(continue-value ...)
                      (apply append (map (lambda (f) (f 'step)) clauses))]
                     [repeat-label (new-sym s-k "LOOP-REPEAT")]
                     [recur-label s-recur-name]
                     [(inner-body ...) (loop-codegen-body clauses)]
                     [epilogue (loop-codegen-epilogue s-k clauses)])
                  #'(let recur-label ([recur-binding-vars recur-binding-values] ...)
                      (let* ([before-loop-begin-vars before-loop-begin-values] ...)
                        (let repeat-label ([binding-vars binding-values] ...)
                          (if (and continue-condition ...)
                              (let* bindings-on-loop-begin
                                inner-body ...
                                (repeat-label continue-value ...)))))
                      epilogue))
                (car args))
               ]
              [(step)
               (list)]
              [(finally)
               (list)]
              [else (syntax-violation #'make-loop-plugin "never goes here" method)]))))))

  (define (loop/core/loop original-e)
    (let loop ([e original-e])
      (syntax-case e (:name :loop)
        [(k (:loop :name <var> clauses ...) rest ...)
         (identifier? #'<var>)
         (with-syntax ([next-k (loop-level++ #'k)])
           (values (make-loop-plugin #'k #'(next-k clauses ...) #'<var>)
                   #'(k rest ...)))
         ]
        [(k (:loop clauses ...) rest ...)
         (with-syntax ([name (new-sym #'k "RECUR")])
           (loop #'(k (:loop :name name clauses ...) rest ...)))
         ]
        [(k rest ...)
         (values #f e)
         ]))))
