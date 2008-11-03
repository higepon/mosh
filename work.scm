(import (rnrs)
        (mosh))
;; ;; (display (/ 1 3))
;; ;; (display (/ (inexact 2) 3))
;; ;; (newline)
;; ;(display (inexact (* 1000000 10000000 10000000 10000000)))
;; (display (/ 1 (+ (greatest-fixnum) 1)))
;; (newline)
;; (display (/ (/ 1 2) (+ (greatest-fixnum) 1)))

;; (newline)

;; (display (/ (inexact (/ 1 3)) (+ (greatest-fixnum) 1)))

;; (newline)

;; (display (= (/ (+ (greatest-fixnum) 1) 1) (+ (greatest-fixnum) 1)))

;; (newline)
;; (display (/ (+ (greatest-fixnum) 1) (/ 1 3) ))

;; (newline)
;; (display (/ (+ (greatest-fixnum) 1) (inexact (/ 1 3) )))


;; (display (fixnum? (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1))))


;; (newline)
;; (newline)
;; (display (fixnum? (/ (+ (greatest-fixnum) 1) 1)))
;; (display (fixnum? (+ (greatest-fixnum) 1)))
;; (display (rational? (/ (+ (greatest-fixnum) 1) 1)))
;; (display (rational? (+ (greatest-fixnum) 1)))
;; ;; (display (bignum? (/ (+ (greatest-fixnum) 1) 1)))
;; ;; (display (bignum? (+ (greatest-fixnum) 1)))

;(display (number? (+ (greatest-fixnum) 1)))
(format #t "~a:\n" (= (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)))
;(format #t "~a:\n" (= (/ (+ (greatest-fixnum) 1) 1) (+ (greatest-fixnum) 1)))
(display (make-complex 3 2))

;; (let loop ([i 0])
;;   (if (> i 10000)
;;       (display "\ndone")
;;       (begin (display i)
;;              (display "\r")
;;              (let busy ([j 0])
;;                (if (> j 1000000)
;;                    '()
;;                    (busy (+ j 1))))
;;              (loop (+ i 1)))))
