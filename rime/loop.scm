#!r6rs
(library (rime loop)
  (export loop
          :for :as :and :from :downfrom :upfrom :to :downto :upto :below :above :by  :collect :count :into :append :do :with :=
          :in :in-string :on :across :in-vector :offset
          :being :each :the :in-hashtable :hash-key :hash-keys :hash-value :hash-values :of :using
          :finally
          :break
          :repeat
          :if :when :unless
          :initially
          :loop
          :name
          :trace-parser
          :trace-codegen
          )
  (import (rnrs (6))
          (rime loop test)
          (rime loop keywords)
          (for (rime loop core) expand)
          (for (rime loop arithmetic) expand)
          (for (rime loop list) expand)
          (for (rime loop vector) expand)
          (for (rime loop string) expand)
          (for (rime loop hashtable) expand)
          (for (rime loop if) expand)
          (for (rime loop do) expand)
          (for (rime loop with) expand)
          (for (rime loop collect) expand)
          (for (rime loop break) expand)
          (for (rime loop finally) expand)
          (for (rime loop initially) expand)
          (for (rime loop loop) expand))
  (use-loop-plugin loop/core/arithmetic)
  (use-loop-plugin loop/core/list)
  (use-loop-plugin loop/core/vector)
  (use-loop-plugin loop/core/string)
  (use-loop-plugin loop/core/hashtable)
  (use-loop-plugin loop/core/if)
  (use-loop-plugin loop/core/do)
  (use-loop-plugin loop/core/with)
  (use-loop-plugin loop/core/collect)
  (use-loop-plugin loop/core/break)
  (use-loop-plugin loop/core/finally)
  (use-loop-plugin loop/core/initially)
  (use-loop-plugin loop/core/loop)
  (define-syntax loop
    (lambda (original-e)
      (let loop ([e original-e]
                 [trace-parser #f]
                 [trace-codegen #f])
        (syntax-case e (:loop :trace-parser :trace-codegen)
          [((k props ...) (:loop clauses ...))
           (let-values ([(clauses s-k)
                         (parse-loop-clauses
                          #'((k props ...)
                             (:loop clauses ...)))])
             (loop-codegen s-k clauses original-e))]
          [(k :trace-parser clauses ...)
           (identifier? #'k)
           (loop #'(k clauses ...) #t trace-codegen)
           ]
          [(k :trace-codegen clauses ...)
           (identifier? #'k)
           (loop #'(k clauses ...) trace-parser #t)
           ]
          [(k clauses ...)
           (identifier? #'k)
           (with-syntax ([k (loop-init-props #'k trace-parser trace-codegen)])
             (loop #'(k (:loop clauses ...)) trace-parser trace-codegen))
           ]
          )))))
