#!r6rs
(library (test repl)
  (export main ll)
  (import (rnrs (6))
          (rime syntax __syntax-location)
          )
  (define-syntax ll
    (lambda (e)
      (syntax-case e ()
        [(log exprs ...)
         (let ([loc (syntax-location #'log)])
           (with-syntax ([file (list-ref loc 0)]
                         [line (list-ref loc 1)]
                         [column (list-ref loc 2)])
             #'(begin
                 (display file)
                 (display ":")
                 (display line)
                 (display ":")
                 (display column)
                 (display ": ")
                 (display exprs) ...
                 )))])))

  (define (main)
    (ll "hello world 1\n")
    (ll "hello world 2\n")
    (ll "hello world 3\n")))
