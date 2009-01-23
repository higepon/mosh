(import (rnrs)
        (dbi)
        (mosh test)
        (clos core))

(define dbi (make <dbi>))

(test* (class-of dbi) <dbi>)

(print-object-with-slots dbi (current-output-port))
