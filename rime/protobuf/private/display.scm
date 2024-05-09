#!r6rs
(library (rime protobuf private display)
  (export
   object-to-string
   display-objects)
  (import (rnrs (6)))
  (define (object-to-string . args)
    (call-with-string-output-port
     (lambda (port)
       (let loop ([args args])
         (unless (null? args)
           (let ([arg (car args)])
             (cond
              [(and (pair? arg)
                    (eq? '~s (car arg)))
               (put-datum port (cdr arg))]
              [(string? arg) (put-string port arg)]
              [(char? arg) (put-char port arg)]
              [else (put-datum port arg)])
             (loop (cdr args))))))))

  (define (display-objects . args)
    (display (apply object-to-string args))))
