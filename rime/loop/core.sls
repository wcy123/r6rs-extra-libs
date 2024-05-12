#!r6rs
(library (rime loop core)
  (export
   parse-loop-clauses
   loop-codegen
   )
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (rime loop keywords)
          (rime pretty-print)
          (rime loop arithmetic)
          (rime loop list)
          (rime loop vector)
          (rime loop string)
          (rime loop hashtable)
          (rime loop if)
          (rime loop do)
          (rime loop with)
          (rime loop collect)
          (rime loop break)
          (rime loop finally)
          (rime loop initially)
          )

  (define (all-plugins)
    (list (cons 'loop/core/arithmetic loop/core/arithmetic)
          (cons 'loop/core/list loop/core/list)
          (cons 'loop/core/vector loop/core/vector)
          (cons 'loop/core/string loop/core/string)
          (cons 'loop/core/hashtable loop/core/hashtable)
          (cons 'loop/core/if loop/core/if)
          (cons 'loop/core/do loop/core/do)
          (cons 'loop/core/with loop/core/with)
          (cons 'loop/core/collect loop/core/collect)
          (cons 'loop/core/break loop/core/break)
          (cons 'loop/core/finally loop/core/finally)
          (cons 'loop/core/initially loop/core/initially)
          (cons 'loop/core/loop loop/core/loop)))

  (define (parse-loop-clauses original-e)
    (let ([clauses '()])
      (define (add-clause c)
        (set! clauses (cons c clauses)))
      (define (parse-clause e)
        (let loop-plugin ([plugins (all-plugins)])
          (if (null? plugins)
              (values #f e)
              (let ([plugin (car plugins)])
                (let ([plugin-name (car plugin)]
                      [plugin-func (cdr plugin)])
                  (let-values ([(clause rest) (plugin-func e)])
                    (if clause
                        (begin
                          (when (loop-trace-parser e)
                            (display-objects
                             "rime/loop/core.sls:55:30: [" plugin-name "]"
                             " produces [" (clause 'debug) "]"
                             " rest=" (syntax->datum rest) "\n"
                             " e=" (syntax->datum e) "\n"))
                          (values clause rest))
                        (loop-plugin (cdr plugins)))))))))
      (let loop ([e original-e])
        (let-values ([(clause rest) (parse-clause e)])
          (cond
           [clause
            (cond
             [(procedure? clause)
              (add-clause clause)
              (loop rest)]
             [else (loop rest)])]
           [else
            (syntax-case e ()
              [((k props ...))
               (when (or #f (loop-trace-parser #'(k props ...)))
                 (display-objects
                  "rime/loop/core.sls:78:19: PARSING OK! "
                  (syntax->datum original-e) " => clauses: "
                  (loop-clauses-to-string (loop-level #'(k props ...))
                                          (reverse clauses))
                  "\n")
                 )
               (values (reverse clauses) #'(k props ...))]
              [_else (syntax-violation
                      'parse-loop-clauses
                      (call-with-string-output-port
                       (lambda (port)
                         (put-string port "loop parse1 ")
                         (put-datum port (syntax->datum original-e))
                         (put-string port " error at")))
                      #'original-e
                      e)]
              )]
           )))))
  (define (codegen-prologue-binding k clauses)
    (define (remove-weak bindings)
      (let ([vars '()]
            [values '()]
            [weaks '()])
        (define (push var value weak)
          (set! vars (cons var vars))
          (set! values (cons value values))
          (set! weaks (cons weak weaks)))
        (define (find-binding var)
          (let repeat ([vars vars]
                       [values values]
                       [weaks weaks])
            (if (not (null? vars))
                (if (bound-identifier=? (car vars) var)
                    (list vars values weaks)
                    (repeat (cdr vars) (cdr values) (cdr weaks)))
                #f)))
        (define (process-binding binding)
          (let repeat ([binding binding])
            (syntax-case binding ()
              [(name value)
               (repeat #'(name value #f))]
              [(name value weak)
               (let ([weak (syntax->datum #'weak)]
                     [var #'name]
                     [value #'value])
                 (let ([previous-binding (find-binding var)])
                   (cond
                    [(not previous-binding)
                     (push var value weak)]
                    [(not weak) ;; overwrite previous binding
                     (set-car! (list-ref previous-binding 1) value)
                     (set-car! (list-ref previous-binding 2) weak)
                     ]
                    [(car (list-ref previous-binding 2))
                     ;; previous also weak binding
                     (set-car! (list-ref previous-binding 1) value)
                     (set-car! (list-ref previous-binding 2) weak)
                     ]
                    ))
                 )])))
        (let repeat ([bindings  bindings])
          (cond
           [(null? bindings)
            (reverse (map list vars values))]
           [else (process-binding (car bindings))
                 (repeat (cdr bindings))]
           ))))
    (remove-weak
     (with-syntax ([loop-return-value (loop-return-value k)])
       (apply append
              (list #'(loop-return-value (if #f 0) #t))
              (map (lambda (loop-clause)
                     (loop-clause 'setup)) clauses)))))

  (define (loop-codegen-body clauses)
    (let loop ([clauses clauses])
      (if (null? clauses)
          '()
          (begin
            ((car clauses) 'loop-body (loop (cdr clauses)))))))

  (define (partition-epilogue-clauses clauses)
    (let loop ([pre-clauses '()]
               [clauses clauses])
      (cond
       [(null? clauses)
        (values (reverse pre-clauses) clauses)]
       [((car clauses) 'is-finally?)
        (values (reverse pre-clauses) clauses)]
       [else
        (loop (cons (car clauses) pre-clauses)
              (cdr clauses))])))

  (define (loop-codegen-epilogue k clauses)
    (with-syntax ([(finally-block ...)
                   (apply append (map (lambda (clause) (clause 'finally)) clauses))]
                  [:return-value (loop-return-value k)])
      #'(begin finally-block ... :return-value)))

  (define (loop-codegen k clauses original-e)
    (let ([code
           (with-syntax
               ([(prologue-binding ...) (codegen-prologue-binding k clauses)]
                [(inner-body ...) (loop-codegen-body clauses)]
                [epilogue (loop-codegen-epilogue k clauses)])
             #'(let* (prologue-binding ...)
                 inner-body ...
                 epilogue))])
      (when (loop-trace-codegen k)
        (display-objects
         "rime/loop/core.sls:173:10: [" (syntax->datum original-e) "]"
         " expand to \n"
         (pretty-print-to-string (syntax->datum code))
         "\n"))
      code))
  (define (loop-clauses-to-string level clauses)
    (define space #\ )
    (define (indent level)
      (make-string (fx* 4 level) space))
    (apply string-append (map
                          (lambda (c)
                            (object-to-string
                             "\n" (indent (fx+ level 1)) "LOOP[" level "] "
                             (c 'debug)))
                          clauses)))

  (define (display-loop-plugin level s-loop-expr clauses)
    (object-to-string
     ":loop@" level " "
     (syntax->datum s-loop-expr)
     (loop-clauses-to-string (fx+ level 1) clauses)))

  (define (make-loop-plugin s-k s-loop-expr s-recur-name)
    (let-values ([(clauses s-next-k) (parse-loop-clauses s-loop-expr)])
      (let-values ([(clauses epilogue-clauses) (partition-epilogue-clauses clauses)])
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
                       [epilogue (loop-codegen-epilogue s-k epilogue-clauses)])
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
                [(is-finally?) #f]
                [(finally)
                 (list)]
                [else (syntax-violation #'make-loop-plugin "never goes here" method)])))))))

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
