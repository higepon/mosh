(import (rnrs)
        (srfi :98 os-environment-variables))

(display (get-environment-variable "HOME"))
