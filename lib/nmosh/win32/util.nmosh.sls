(library (nmosh win32 util)
         (export null-pointer
                 string->utf16-bv
                 mbcs->string
                 mbcs->string/cp)
         (import (rnrs)
                 (nmosh win32 misc)
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

(define (string->utf16-bv str) ;; N.B.: adds null char
  (define str-bv (string->bytevector str (make-transcoder (utf-16-codec))))
  (define len (bytevector-length str-bv))
  (define ret (make-bytevector (+ len 2) 0))
  (bytevector-copy! str-bv 0 ret 0 len)
  (byteswap! ret))

(define (mbcs->string/cp cp bv)
  (define (convert)
    (let* ((len (win32_measure_multibyte_to_widechar cp bv))
           (out-bv (make-bytevector len)))
      (let ((r (win32_multibyte_to_widechar cp bv out-bv)))
        (when (= r 0)
          (assertion-violation 'mbcs->string/cp
                               "something wrong"))
        (utf16->string out-bv (endianness little)))))
  (if (= 0 (bytevector-length bv))
    ""
    (convert)))

(define (mbcs->string bv)
  (mbcs->string/cp (win32_get_ansi_codepage) bv))

)
