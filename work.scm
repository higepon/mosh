(import (rnrs)
        (mosh file)
        (srfi :98))

(display (get-environment-variable "HOME"))
(newline)
;(display (file-newer? "lib/srfi/%3A98.ss.fasl" "lib/srfi/%3A98.ss"))
