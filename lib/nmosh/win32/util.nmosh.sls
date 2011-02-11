(library (nmosh win32 util)
         (export null-pointer
                 string->utf16-bv)
         (import (rnrs)
                 (mosh ffi))

(define null-pointer (integer->pointer 0))

(define (byteswap! bv)
  (define len (bytevector-length bv))
  (define (itr idx)
    (if (>= idx len)
      bv
      (begin
        (let ((i (bytevector-u16-ref bv idx (endianness big))))
          (bytevector-u16-set! bv idx i (endianness little)))
        (itr (+ 2 idx)))))
  (itr 0))

(define (string->utf16-bv str)
  (define str-bv (string->bytevector str (make-transcoder (utf-16-codec))))
  (define len (bytevector-length str-bv))
  (define ret (make-bytevector (+ len 2) 0))
  (bytevector-copy! str-bv 0 ret 0 len)
  (byteswap! ret))

)
