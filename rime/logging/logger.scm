#!r6rs
(library (rime logging logger)
  (export logger
          :source-location
          :who
          :level
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
    :source-location
    :who
    :level
    :trace
    :info
    :debug
    :warning
    :error
    :critical
    )
  (define-syntax logger
    (lambda (e)

      (syntax-case e (:header
                      :level-label
                      :level-number
                      :source-location
                      :who
                      :trace
                      :info
                      :debug
                      :warning
                      :error
                      :critical
                      )
        [(logger expr exprs ...)
         (not (free-identifier=? #'expr #':header))
         #'(logger :header ((:source-location . logger)
                            (:who . logger)
                            (:level-number . 400)
                            (:level-label . "I")
                            ) expr exprs ...)]

        [(logger :header ((k . v) ...) :trace exprs ...)
         #'(logger :header ((:level-number . 500)
                            (:level-label . "T") (k . v) ...) exprs ...)]

        [(logger :header ((k . v) ...) :error exprs ...)
         #'(logger :header ((:level-number . 100)
                            (:level-label . "E") (k . v) ...) exprs ...)]

        [(logger :header ((k . v) ...) :who who exprs ...)
         #'(logger :header ((:who . who) (k . v) ...) exprs ...)]

        [(logger :header ((k . v) ...) :source-location loc exprs ...)
         #'(logger :header ((:source-location . loc) (k . v) ...) exprs ...)]

        [(logger :header ((k . v) ...)  exprs ...)
         (let ([header #'((k . v) ...)])
           (define (assq key)
             (cdr (assp (lambda (k) (free-identifier=? k key)) header)))
           (define (source-location)
             (call-with-string-output-port
               (lambda (port)
                 (let ([s-loc (syntax-location (assq #':source-location))])
                   (let ([file (list-ref s-loc 0)]
                         [line (list-ref s-loc 1)]
                         [column (list-ref s-loc 2)])
                     (display file port)
                     (display ":" port)
                     (display line port)
                     (display ":" port)
                     (display column port)
                     (display ": " port))))))
           (with-syntax ([level-number (assq #':level-number)]
                         [level-label (assq #':level-label)]
                         [file:line:column (source-location)])
             #'(begin
                 (let ([port (current-output-port)])
                   (display file:line:column port)
                   (display level-label port)
                   (display " " port)
                   (display exprs port) ...
                   (newline port)))))]
        ))))
