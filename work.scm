(import (rnrs)
        (mosh test))

(bytevector->string #vu8(#xFE #xFF)
                                   (make-transcoder (utf-16-codec)))

(let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                'block (make-transcoder (utf-16-codec)))])
  (put-string p "app\x3BB;e")
  (close-port p))
(test-end)
