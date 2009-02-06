(import (rnrs)
        (srfi :0)
        (srfi :1)
        (mosh test))

;;;;;  SRFI-0   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond-expand
 (mosh
   (define name 'mosh))
 (else
  (define name 'other)))

(test* name 'mosh)

;;;;;  SRFI-1   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (first '(1 2)) 1)
(test* (second '(1 2)) 2)

(test-end)


