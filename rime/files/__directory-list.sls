#!r6rs
(library (rime files __directory-list)
  (export directory-list)
  (import (rime rime-0)
          (rnrs (6))
          (rnrs eval (6))
          )
  (cond-expand
   [(library (chezscheme))
    (define directory-list (eval 'directory-list (environment '(chezscheme))))]
   [(library (ice-9 ftw))
    (define scandir (eval 'scandir (environment '(ice-9 ftw))))
    (define (directory-list directory-name)
      (scandir directory-name (lambda (name)
                                (cond
                                 [(string=? name ".") #f]
                                 [(string=? name "..") #f]
                                 [else #t]))))]
   [else
    (define (directory-list diretory-name)
      (raise
       (condition
        (make-error)
        (make-who-condition 'diretory-list)
        (make-message-condition "not implemented")
        )))]))
