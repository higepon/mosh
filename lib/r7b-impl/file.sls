#!r6rs
(library (r7b-impl file)
         (export 
;; from R7RS draft 4
call-with-input-file call-with-output-file delete-file file-exists?
open-binary-input-file open-binary-output-file open-input-file
open-output-file with-input-from-file with-output-to-file
   )
         (import (rnrs))
(define (open-binary-input-file file)
  (open-file-input-port file))

(define (open-binary-output-file file)
  (open-file-output-port file))

)
