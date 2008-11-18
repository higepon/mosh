(define (mod x y)
  (- x (* (div x y) y)))

(define (div-and-mod x y)
  (let ([d (div x y)])
      (values d (- x (* d y)))))

(define (mod0 x y)
    (- x (* (div0 x y) y)))

(define (div0-and-mod0 x y)
    (let ([d0 (div0 x y)])
      (values d0 (- x (* d0 y)))))

;; N.B. We can implement much faster version for Bignum using GMP.
(define (gcd2 m n)
  (if (zero? n)
      (abs m)
      (gcd2 n (mod m n))))

;; (define (gcd . n*)
;; ;;   (unless (for-all integer-valued? n*)
;; ;;     (assertion-violation 'gcd "integer valued numbers required"))
;;   (let1 len (length n*)
;;     (cond
;;      [(= 0 len) 0]
;;      [(= 1 len) (abs (first n*))]
;;      [(= 2 len) (gcd2 (first n*) (second n*))]
;;      [else
;;       (apply gcd (gcd2 (first n*) (second n*)) (cddr n*))])))

(define (gcd . n*)
  (unless (for-all integer-valued? n*)
    (assertion-violation 'gcd "integer valued numbers required"))
  (case (length n*)
    [(0) 0]
    [(1) (abs (first n*))]
    [(2) (gcd2 (first n*) (second n*))]
    [else
     (apply gcd (gcd2 (first n*) (second n*)) (cddr n*))]))
