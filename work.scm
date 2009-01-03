(import (rnrs))

(define cont #f)
(call/cc
 (lambda (k)
   (set! cont k)))
(display "hige")
(cont 0)
