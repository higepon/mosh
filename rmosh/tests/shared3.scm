(import (rnrs)
        (r6rs_mode)
        (mosh test))

;; external library is strict #!r6rs mode
;; but shared structure is allowed here.
'#1=(a . #1#)
(test-results)
