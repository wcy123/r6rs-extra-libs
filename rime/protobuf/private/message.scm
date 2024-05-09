(library (rime protobuf private message)
  (export make-message
          protobuf-message
          register-message-ctor
          invoke-message
          make-protobuf-meta-message
          )
  (import (rnrs (6))
          (rime loop)
          (rime protobuf private display)
          )
  (define the-meta-messages (make-eq-hashtable))

  (define-record-type protobuf-meta-message
    (fields make deserialize))

  (define-record-type protobuf-message
    (fields meta))


  (define (make-message type)
    (let ([key (type->key type)])
      (let ([meta (hashtable-ref the-meta-messages key #f)])
        (and meta
             (method-dispatcher ((protobuf-meta-message-make meta)) meta)))))

  (define (method-dispatcher a-concrete-message meta)
    (lambda (method . args)
      (case method
        [(:self)
         a-concrete-message]
        [(:debug-string)
         (object-to-string a-concrete-message)]
        [(:<<)
         (when (null? args)
           (assertion-violation 'deserialize-message "wrong number of argument"))
         (display-objects
          "rime/protobuf/private/message.scm:39:27:"
          " meta = " meta "\n")
         ((protobuf-meta-message-deserialize meta) a-concrete-message (car args))]
        [else (assertion-violation 'protobuf-message-method
                                   "unknown method, valid methods: :debug-string :<< "
                                   method)])))
  (define (invoke-message a-concrete-message)
    (method-dispatcher
     a-concrete-message
     (protobuf-message-meta a-concrete-message)))

  (define (register-message-ctor type a-protobuf-meta-message)
    (display-objects "rime/protobuf/private/message.scm:54:22: "
                     " add a new protobuf message type=" type "\n")
    (hashtable-set! the-meta-messages (type->key type) a-protobuf-meta-message))

  (define (type->key type)
    (string->symbol
     (apply
      string-append
      (loop :initially sep := ""
            :for t :in type
            :collect sep
            :collect (symbol->string t)
            :with sep := "-")))))
