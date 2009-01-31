(import (rnrs))

(define-record-type a
 (sealed #t))

(define-record-type b
 (parent a))

(make-b)
