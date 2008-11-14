(import (rnrs))

;; (display (flonum? +nan.0))
;; (display (flonum? (/ (inexact 0) (inexact 0))))

;; (display 2/3)
;; (display 10.45)

(display (flonum? .45))
(display (flonum? (inexact 45/100)))

(fl=? .45 (inexact 45/100))

(display 10+inf.0i)
