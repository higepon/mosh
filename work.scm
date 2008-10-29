(import (rnrs)
        (mosh)
        (mosh string)
        (mosh file)
        (rnrs mutable-pairs)
        (srfi :98))




;; (set-symbol-value! 'serialize-library serialize-library)
;; (set-symbol-value! 'load-serialized-library load-serialized-library)

(define load-r6rs-top-level (symbol-value 'load-r6rs-top-level))
(load-r6rs-top-level (car (command-line)) 'compile)

(display (get-environment-variable "PATH"))
