(import (rnrs)
        (srfi :26)
        (srfi :98))
(display (get-environment-variable "HOME"))
(display (map(cut * 2 <>) '(1  3)))


;; (import (rnrs)
;;         (clos user)
;;         (srfi :26)
;;         (mosh string))
