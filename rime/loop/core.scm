#!r6rs
(library (rime loop core)
  (export
   use-loop-plugin
   ;; functions
   parse-loop-clauses
   loop-clauses-to-string
   loop-codegen
   loop-codegen-body
   loop-codegen-epilogue
   )
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (rime loop keywords)
          (for (rime loop plugin-store) expand))

  (define-syntax use-loop-plugin
    (lambda (e)
      (syntax-case e ()
        [(k v)
         (with-syntax ([tmp (car (generate-temporaries (list #'k)))])
          #'(begin
              (define-syntax tmp
                (begin (add-plugin (cons 'v v))
                       (lambda (e) e)))))])))

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
                             "scheme/loop/core.scm:54:30: [" plugin-name "]"
                             " produces [" (clause 'debug) "]"
                             " rest=" (syntax->datum rest) "\n"
                             " e=" (syntax->datum e) "\n"))
                          (values clause rest))
                        (begin
                          (loop-plugin (cdr plugins))))))))))
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
               (when (loop-trace-parser #'(k props ...))
                 (display-objects
                  "scheme/loop/core.scm:75:19: PARSING OK! "
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
                         (put-string port "loop parse ")
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

  (define (loop-codegen-epilogue k clauses)
    (with-syntax ([(finally-block ...) (apply append (map (lambda (clause) (clause 'finally)) clauses))]
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
         "scheme/loop/core.scm:171:18: [" (syntax->datum original-e) "]"
         " expand to \n"
         (syntax->datum code)
         "\n"))
      code)))
