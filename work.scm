;(import (rnrs))
(define (b)
  (display 3 4 5 6)
  7)

(define (a x)
  (b)
  x
  )

;(disasm b)
;(disasm a)

(a 3)

