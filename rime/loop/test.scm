#!r6rs
(library (rime loop test)
  (export run-all-tests check define-test)
  (import (rnrs (6))
          (rime loop display))
  (define _store '())
  (define (all-tests)
    (reverse _store))
  (define (add-test name value)
    ;; (printf "ADD TEST [~s]\n" name)
    (set! _store (cons (cons name value) _store)))

  (define-syntax check
    (syntax-rules ()
      [(_ f actual expected)
       (let ([x expected]
             [y actual]
             )
         (if (f x y)
             (begin
               (and #t
                    (display-objects
                     "CHECK-OK (" 'f " " 'actual " " 'expected ") "
                     " => #t"
                     " actual=" y
                     " expected=" x
                     "\n"
                     ))
               #t)
             (assertion-violation
              'check-failed
              (display-objects
               "check-failed (" 'f " " 'actual " " 'expected ")"
               " => #f"
               " actual=" y " expected=" x
               ))
             ))]))

  (define-syntax define-test
    (syntax-rules ()
      [(k name x xs ...)
       (define name
         (let ([value (lambda () x xs ...)])
           (add-test 'name value)
           value))]))

  (define (run-all-tests)
    (let ((tests (all-tests)))
      (display "START TO RUN ALL TEST\n")
      (for-all run-test tests)))

  (define (run-test test)
    (let ([name (car test)]
          [func (cdr test)]
          [succeeded? #f]
          [exception #f])
      (set! succeeded? (func))
      (display "TEST [")
      (display name)
      (display "]")
      (display (if succeeded? " OK" " FAIL"))
      (display "\n")
      succeeded?)))
