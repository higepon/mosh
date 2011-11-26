#!r6rs
(library (r7b-util bytevector-buffer)
         (export
           get-output-bytevector
           open-output-bytevector
           )
         (import (rnrs) (r7b-util buffer-port))



(define get-output-bytevector %get-buffered-data)

(define open-output-bytevector (%create-buffer open-bytevector-output-port))

)
