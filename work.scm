(import (match) (rnrs))

(display 
 (match "3"
 [(= string->number x)
  (write x)]
 [x 9]))

;; (display 
;;  (match 3
;;  [(and (? number? _) (= (lambda (x) (< x 4)) x))
;;   (write x)]
;;  [x 9]))
