#!r6rs
(library (r6rs cond-expand cond-expand)
  (export cond-expand use rename)
  (import (rnrs (6))
          (rnrs eval (6))
          (for (r6rs cond-expand _cond-expand) expand))

  (define-syntax rename (lambda (x) (syntax-violation 'rename "misplaced aux keyword" x)))

  (define-syntax use
    (syntax-rules (rename)
      [(use import-spec)
       (begin)]
      [(use import-spec (rename id-from id-to) ids ...)
       (begin
         (define id-to (eval 'id-from (environment 'import-spec)))
         (use import-spec ids ...))]
      [(use import-spec id ids ...)
       (use import-spec (rename id id) ids ...)]))

  (define-syntax cond-expand
    (lambda (e)
      (syntax-case e (and or not else library)
        [(cond-expand)
         #'(syntax-violation 'cond-expand "Unfulfilled cond-expand")]

        [(cond-expand (else body ...))
         #'(begin body ...)]

        [(cond-expand ((and) body ...) more-clauses ...)
         #'(begin body ...)]

        [(cond-expand ((and req1 req2 ...) body ...)
                      more-clauses ...)
         #'(cond-expand
            (req1
             (cond-expand
              ((and req2 ...) body ...)
              more-clauses ...)))]

        [(cond-expand ((or) body ...) more-clauses ...)
         #'(cond-expand more-clauses ...)]

        [(cond-expand ((or req1 req2 ...) body ...)
                      more-clauses ...)
         #'(cond-expand
            (req1
             (begin body ...))
            (else
             (cond-expand
              ((or req2 ...) body ...)
              more-clauses ...)))]

        [(cond-expand ((not req) body ...)
                      more-clauses ...)
         #'(cond-expand
            (req
             (cond-expand more-clauses ...))
            (else body ...))]
        ;;
        [(cond-expand ((library import-spec) body ...) more-clauses ...)
         (check-library (syntax->datum #'import-spec))
         #'(begin body ...)
         ]
        [(cond-expand (otherwise ...) more-clauses ...)
         #'(cond-expand more-clauses ...)
         ]
        ))))
