(import (rnrs)
        (mosh string))

;; (display (flonum? +nan.0))
;; (display (flonum? (/ (inexact 0) (inexact 0))))

;; (display 2/3)
;; (display 10.45)

;; (display (flonum? .45))
;; (display (flonum? (inexact 45/100)))

;; (fl=? .45 (inexact 45/100))

;; (display 10+inf.0i)
;; (display 10-inf.0i)
;; (newline)
;; (display -i)

  (with-input-from-file "./hage.scm"
    (lambda ()
      (let loop ([obj (read)]
                 [ret '()])
        (cond
         [(eof-object? obj)
          (reverse ret)]
         [else
          (write obj)
          (loop (read) (cons obj ret))]))))
;(newline)
