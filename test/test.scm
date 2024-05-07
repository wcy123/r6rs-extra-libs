#!r6rs
(import (rnrs (6))
        (rime rime-0)
        (rime os environ))

(display "Hello ")
(display (getenv "USER"))
(display "\n")
