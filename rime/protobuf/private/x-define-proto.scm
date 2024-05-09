#!r6rs
(library (rime protobuf private x-define-proto)
  (export x-define-proto)
  (import (rnrs (6))
          (rime protobuf private record)
          (rime protobuf private x-define-proto-record)
          (rime protobuf private x-define-proto-deserialize)
          (rime protobuf private display)
          ;; (ice-9 pretty-print)
          )
  (define (x-define-init r-proto)
    (with-syntax ([init-name  (r-proto-lib-init-name r-proto)])
      ;; chezscheme requires refering a identifier to initialize a library
      #'(define init-name #f)))
  (define (x-define-proto e)
    (let ([proto (make-r-proto e)])
      (let ([output
             #`(begin
                 #,(x-define-init proto)
                 #,@(apply append (map r-field.define-derialize (r-proto-messages proto)))
                 #,@(r-proto.define-record-type proto)
                 )])
        (and #t (display-objects
          "rime/protobuf/private/x-define-proto.scm:24:10: (x-define-proto)"
          " output:\n"
          ;; (call-with-string-output-port
          ;;  (lambda (port)
          ;;    (pretty-print (syntax->datum output) port #:width 120)))
          (syntax->datum output)
          "\n"
          ))
        output))))
