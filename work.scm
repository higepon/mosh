(import (rnrs)
        (mosh test))
(string->bytevector "a\x185;b" (make-transcoder (latin-1-codec) 'lf 'raise))

(test-end)
