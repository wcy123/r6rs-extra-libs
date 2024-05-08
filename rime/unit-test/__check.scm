#!r6rs
(library (rime unit-test __check)
  (export CHECK)
  (import (rnrs (6))
          (rime logging))
  (define-syntax CHECK
    (lambda (e)
      (syntax-case e ()
        [(_CHECK who f actual-expr expected-expr)
         #'(let ([expected-value expected-expr]
               [actual-value actual-expr]
               )
           (if (f expected-value actual-value)
               (begin
                 (logger :trace
                         :source-location _CHECK
                         "CHECK-OK (" 'f " " 'actual-expr " " 'expected-expr ") "
                         " actual = " actual-value
                         "; expected = " expected-value
                         )
                 #t)
               (begin
                 (logger :error
                         :who who
                         :source-location _CHECK
                         "CHECK-FAILED (" 'f " " 'actual-expr " " 'expected-expr ")"
                         " actual = " actual-value "; expected = " expected-value
                         )
                 (assertion-violation
                  'CHECK-failed "check failed"
                  (list (cons ' EXPECTED expected-value)
                        (cons 'ACTUAL actual-value))))
               ))]))))
