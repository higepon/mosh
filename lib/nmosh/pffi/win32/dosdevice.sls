(library (nmosh pffi win32 dosdevice)
         (export win32_query_dosdevice)
         (import (nmosh pffi win32 util)
                 (nmosh ffi box)
                 (rnrs)
                 (prefix (nmosh stubs win32-misc) stub:))

(define (win32_query_dosdevice dev)
  (let ((name (string->utf16-bv dev))
        (buf (make-bytevector 512)))
    (let ((res (stub:win32_querydosdevice name buf 256)))
      (if (= res 0)
        #f
        (utf16-bv->string buf)))))

)
