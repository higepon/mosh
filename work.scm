(import (rnrs)
#;        (mosh test))

(display (string=? (bytevector->string #vu8(97 10 98 13 99 13 10 100 194 133 101 226 128 168 102 13 194 133 103) (make-transcoder (utf-8-codec) 'lf))
        "a\nb\nc\nd\ne\nf\ng"))

;(test-end)
