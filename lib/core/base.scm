(define (exact-integer-sqrt k)
  (unless (and (integer? k) (>= k 0))
    (assertion-violation 'exact-integer-sqrt "exact integer number required" (list k)))
  (let* ([s (exact (truncate (sqrt k)))]
         [r (- k (* s s))])
    (values s r)))

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

;; Originally from Ypsilon Scheme start
(define (rationalize x e)
  (or (real? x) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 1" x) (list x e)))
  (or (real? e) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 2" e) (list x e)))
  (cond ((infinite? e)
         (if (infinite? x) +nan.0 0.0))
        ((= x 0) x)
        ((= x e) (- x e))
        ((negative? x)
         (- (rationalize (- x) e)))
        (else
         (let ((e (abs e)))
           (let loop ((bottom (- x e)) (top (+ x e)))
             (cond ((= bottom top) bottom)
                   (else
                    (let ((x (ceiling bottom)))
                      (cond ((< x top) x)
                            (else
                             (let ((a (- x 1)))
                               (+ a (/ 1 (loop (/ 1 (- top a)) (/ 1 (- bottom a))))))))))))))))
;; Originally from Ypsilon Scheme end

;; N.B. We can implement much faster version for Bignum using GMP.
(define (gcd2 m n)
  (if (zero? n)
      (if (inexact? n) (inexact (abs m)) (abs m))
      (gcd2 n (mod m n))))

(define (gcd . n*)
  (unless (for-all integer-valued? n*)
    (assertion-violation 'gcd "integer valued numbers required"))
  (case (length n*)
    [(0) 0]
    [(1) (abs (first n*))]
    [(2) (gcd2 (first n*) (second n*))]
    [else
     (apply gcd (gcd2 (first n*) (second n*)) (cddr n*))]))

(define (lcm2 a b)
  (abs (/ (* a b) (gcd2 a b))))

(define (lcm . n*)
  (unless (for-all integer-valued? n*)
    (assertion-violation 'lcm "integer valued numbers required"))
  (case (length n*)
    [(0) 1]
    [(1) (abs (first n*))]
    [(2) (lcm2 (first n*) (second n*))]
    [else
     (apply lcm (lcm2 (first n*) (second n*)) (cddr n*))]))
