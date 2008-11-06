;; (import (rnrs)
;;         (mosh))

;(display (/ (make-complex 1 2) (make-complex 1 2)))

;; (display (least-fixnum))
;; (display (+ (- 0 (+ (greatest-fixnum) 1)) 1))
;; (display (- (make-complex 1 1) (+ (greatest-fixnum) 2)));(display (- (make-complex 1 1) 2))
;(display (make-complex (/ (greatest-fixnum) 2) (- 0 (/ (greatest-fixnum) 2))))
;(display (/ (+ (greatest-fixnum) 1) (make-complex 1 1)))
;(newline)

(define (fxzero2? x) (= 0 x))
(define (fxsub1 x) (- x 1))
(letrec ([e (lambda (x) (if (fxzero2? x) #t (o (fxsub1 x))))]        [o (lambda (x) (if (fxzero2? x) #f (e (fxsub1 x))))])  (e 5000000))


;(letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))]        [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 5000000))

(letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))] 
         [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 50000))

