(import (rnrs)
        (srfi :0)
        (srfi :42))

(cond-expand
 (mosh
   (define name 'mosh))
 (else
  (define name 'other)))

(display name)
(newline)
(display  (list-ec (: i 5) (* i i)))
(newline)
