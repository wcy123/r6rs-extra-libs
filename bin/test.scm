#!r6rs
(import (rnrs (6))
        (rnrs eval (6))
        (rime loop)
        (rime logging))
(logger :info "START BEGIN " (command-line))
(let* ([import-spec
       (loop :for arg :in (cdr (command-line))
             :collect (string->symbol arg))]
      [expr `(import ',import-spec)])
  (logger :info "running " import-spec)
  (eval '(main) (environment import-spec))
  (logger :info "running " expr)
  )
