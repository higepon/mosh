(import (rnrs)
        (mosh test))


(display (bytevector->string #vu8(254 255 0 97 0 112 0 112 3 187 0 101) (make-transcoder (utf-16-codec))))
