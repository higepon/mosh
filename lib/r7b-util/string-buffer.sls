#!r6rs
(library (r7b-util string-buffer)
         (export
           get-output-string
           open-output-string
           )
         (import (rnrs) 
                 (r7b-util buffer-port))


(define get-output-string %get-buffered-data)
(define open-output-string (%create-buffer open-string-output-port))

)
