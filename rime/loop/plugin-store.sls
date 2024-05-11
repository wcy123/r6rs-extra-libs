#!r6rs
(library (rime loop plugin-store)
  (export add-plugin all-plugins)
  (import (rnrs (6)))
  (define _store '())
  (define (all-plugins)
    (reverse _store))
  (define (add-plugin plugin)
    (let ([name  (car plugin)]
          [value (cdr plugin)])
      (set! _store (cons (cons name value) (all-plugins))))))
