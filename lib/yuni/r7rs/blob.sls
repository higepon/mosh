;; R7RS draft1 6.3.7 blobs
(library (yuni r7rs blob)
         (export
           blob?
           make-blob
           blob-length
           blob-u8-ref
           blob-u8-set!
           blob-copy
           blob-copy!
           partial-blob
           partial-blob-copy!)
         (import (rnrs))
(define blob? bytevector?)
(define (make-blob k)
  (make-bytevector k))
(define blob-length bytevector-length)
(define blob-u8-ref bytevector-u8-ref)
(define blob-u8-set! bytevector-u8-set!)
(define blob-copy bytevector-copy)
(define (blob-copy! from to)
    (bytevector-copy! from 0 to 0 
                      (bytevector-length from)))
(define (partial-blob blob start end)
  (let* ((len (+ (- end start) 1))
         (bv (make-bytevector len)))
    (bytevector-copy! blob start bv 0 len)
    bv))
(define (partial-blob-copy! from start end to at)
  (let ((len (+ (- end start) 1)))
    (bytevector-copy! from start to at len)))

)


