;; (import (except (rnrs) gcd)
;;         (srfi :1)
;;         (mosh string))

;; ;; N.B. We can implement much faster version for Bignum using GMP.
;; (define (gcd2 m n)
;;   (if (zero? n)
;;       (abs m)
;;       (gcd2 n (mod m n))))

;; (define (gcd . n*)
;;   (unless (for-all integer-valued? n*)
;;     (assertion-violation 'gcd "integer valued numbers required"))
;;   (case (length n*)
;;     [(0) 0]
;;     [(1) (abs (first n*))]
;;     [(2) (gcd2 (first n*) (second n*))]
;;     [else
;;      (apply gcd (gcd2 (first n*) (second n*)) (cddr n*))]))
;; (define (gcd2 m n)
;;   (if (zero? n)
;;       (abs m)
;;       (gcd2 n (mod m n))))

;; (define (gcd1 . n)
;;     (cond
;;      [(= (length n) 2) (display 'hoge) (gcd2 (first n) (second n))]
;;     [else
;;      (apply gcd (gcd2 (first n) (second n)) (cddr n))]))
(define (gcd2 m n)
  (if (zero? n)
      (abs m)
      (gcd2 n (mod m n))))

(define (gcd n m)
)
;;   (unless (for-all integer-valued? n*)
;;     (assertion-violation 'gcd "integer valued numbers required"))
;;   (case (length n*)
;;     [(0) 0]
;;     [(1) (abs (first n*))]
;;     [(2) (gcd2 (first n*) (second n*))]
;;     [else
;;      (apply gcd (gcd2 (first n*) (second n*)) (cddr n*))]))

(display (gcd 16 4))
(newline)

