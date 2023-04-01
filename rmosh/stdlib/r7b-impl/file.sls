#!r6rs
(library (r7b-impl file)
         (export 
;; from R7RS draft 4
call-with-input-file call-with-output-file delete-file file-exists?
open-binary-input-file open-binary-output-file open-input-file
open-output-file with-input-from-file with-output-to-file
   )
         (import (rename (rnrs) (delete-file r6rs:delete-file) (open-input-file r6rs:open-input-file)))
(define (open-binary-input-file file)
  (open-file-input-port file))

(define (open-binary-output-file file)
  (open-file-output-port file))

; TODO: Better i/o error layer between R6RS <=> R7RS.
(define (delete-file file)
  (guard (exn (else (raise (make-i/o-read-error))))
    (r6rs:delete-file file)))

; TODO: Better i/o error layer between R6RS <=> R7RS.
(define (open-input-file file)
  (guard (exn (else (raise (make-i/o-read-error))))
    (r6rs:open-input-file file)))

)
