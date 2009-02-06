(import (except (rnrs) let-values)
        (srfi :0)
        (srfi :1)
        (srfi :11)
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

;;;;;  SRFI-11   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (let-values (([x y] (values 1 2)))
         (- x y)) -1)



(test-end)


