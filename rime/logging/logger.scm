#!r6rs
(library (rime logging logger)
  (export logger
          :trace
          :info
          :debug
          :warning
          :error
          :critical
          )
  (import (rnrs (6))
          (rime syntax __syntax-location)
          (rime syntax __define-auxiliary-syntax))
  (define-auxiliary-syntax
    :trace
    :info
    :debug
    :warning
    :error
    :critical
    )
  (define-syntax logger
    (lambda (e)
      (syntax-case e (:trace
                      :info
                      :debug
                      :warning
                      :error
                      :critical
                      )
          [(logger :trace exprs ...)
           (let ([loc (syntax-location #'logger)])
             (with-syntax ([file (list-ref loc 0)]
                           [line (list-ref loc 1)]
                           [column (list-ref loc 2)])
               #'(begin
                   (display file)
                   (display ":")
                   (display line)
                   (display ":")
                   (display column)
                   (display ":")
                   (begin ;; (display " ")
                          (display exprs)) ...
                   (newline)
                   )))]))))
