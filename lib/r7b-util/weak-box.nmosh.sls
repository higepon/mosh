(library (r7b-util weak-box)
         (export make-weak-box
                 weak-box-ref
                 weak-box-set!)
         (import (rnrs) (nmosh weak-vectors))
(define (make-weak-box)
  (make-weak-vector 1))
(define (weak-box-ref wb)
  (weak-vector-ref wb 0))
(define (weak-box-set! wb obj)
  (weak-vector-set! wb 0 obj))
)
