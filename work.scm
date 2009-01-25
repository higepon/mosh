(import (rnrs)
        (mosh)
        (rnrs eval)
        (dbi))

((symbol-value 'eval-r6rs) '(import (dbd mysql)))
(define dbi ((symbol-value 'eval-r6rs) '(make-dbi)) )

(display (dbd-connect dbi))

