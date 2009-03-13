(import (rnrs)
        #;(mosh test))


(display (i/o-encoding-error-char (make-i/o-encoding-error 'port 'char)))
;(bytevector->string #vu8(97 #xff 98 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
