#!r6rs
(library (r7b-util bytevector-buffer)
         (export
           get-output-bytevector
           open-output-bytevector
           )
         (import (rnrs) 
                 (primitives sys-get-bytevector
                             sys-open-bytevector-output-port))

(define get-output-bytevector sys-get-bytevector)
(define (open-output-bytevector) (sys-open-bytevector-output-port))

)
