#!r6rs
(import (rnrs (6))
        (test repl)
        (rime rime-0)
        (rime rime-98))
(display (string-append "HOME=" (get-environment-variable "HOME") "\n"))
(main)
