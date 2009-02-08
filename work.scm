(import (srfi :0)
        (only (rnrs)define else quote display))

(cond-expand
 (mosh
   (define name 'moshs))
 (else
  (define name 'other)))

(display name)
