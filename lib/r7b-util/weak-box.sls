(library (r7b-util weak-box)
         (export make-weak-box
                 weak-box-ref
                 weak-box-set!)
         (import (rnrs) (rnrs mutable-pairs))
         
(define (make-weak-box) (cons #f #f))
(define (weak-box-ref wb) (car wb))
(define (weak-box-set! wb obj) (set-car! wb obj))
         
(display "WARNING: Using FAKE implementation of weak-box\n" 
         (current-error-port))
(display "WARNING: MEMORY LEAK will occur\n"
         (current-error-port))
)
