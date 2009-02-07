(import (srfi :0)
        (only (rnrs)define else quote))

(cond-expand
 (mosh
   (define name 'mosh))
 (else
  (define name 'other)))
