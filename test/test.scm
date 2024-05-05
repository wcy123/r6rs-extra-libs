#!r6rs
(import (rnrs (6))
        (r6rs cond-expand)
        (r6rs os environ))

(cond-expand
 [(library (ice-9 pretty-print))
  (use (ice-9 pretty-print)
       (rename pretty-print $pretty-print))]
 [(library (chezscheme))
  (use (chezscheme)
       (rename pretty-print $pretty-print))]
 [else (define $pretty-print #f)])

(define pretty-print
  (if (procedure? $pretty-print)
      (lambda  (expr)
        (call-with-string-output-port
         (lambda (port)
           ($pretty-print expr port))))
     display))

(display (pretty-print '(hello world)))

(display "Hello ")
(display (getenv "USER"))
(display "\n")
