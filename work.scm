(import (rnrs))

(let* ([binary-port (open-bytevector-input-port #vu8(97 98 99))]
       [text-port (transcoded-port binary-port (make-transcoder (latin-1-codec)))])
  (display (read-char text-port))
  (display (read-char text-port))
  (get-u8 binary-port)
  (display (read-char text-port)))
