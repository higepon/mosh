(import (rnrs)
        (mosh))

;(display (/ (make-complex 1 2) (make-complex 1 2)))

;; (display (least-fixnum))
;; (display (+ (- 0 (+ (greatest-fixnum) 1)) 1))
;; (display (- (make-complex 1 1) (+ (greatest-fixnum) 2)));(display (- (make-complex 1 1) 2))
;(display (make-complex (/ (greatest-fixnum) 2) (- 0 (/ (greatest-fixnum) 2))))
;(display (/ (+ (greatest-fixnum) 1) (make-complex 1 1)))
;(newline)

;; (define (fxzero2? x) (= 0 x))
;; (define (fxsub1 x) (- x 1))
;; (letrec ([e (lambda (x) (if (fxzero2? x) #t (o (fxsub1 x))))]        [o (lambda (x) (if (fxzero2? x) #f (e (fxsub1 x))))])  (e 5000000))


;(letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))]        [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 5000000))

;; (letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))] 
;;          [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 50000))

;; (display (letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))]
;;                   [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 2)))

;; (display (let ([plus-inf (/ (inexact 1) (inexact 0))])
;;       plus-inf))

;; (display (inexact 2))
;; (display  (denominator (inexact (/ 6 4))))

;(display (= (/ (inexact 0) (inexact 0)) (flmax (inexact 3) (/ (inexact 0) (inexact 0)))))

;(display (fl+ (/ (inexact 1) (inexact 0)) (inexact 2)))

(define t123.0 (inexact 123))
(define t-123.0 (inexact -123))
(define t10.0 (inexact 10))
(define t-10.0 (inexact -10))
(define t-1.0 (inexact -1))
(define t12.0 (inexact 12))
(define t2.0 (inexact 2))
(define t4.0 (inexact 4))
(define t-12.0 (inexact -12))

(define t7.389 (inexact (/ 7389 1000)))
(define t1024.0 (inexact 1024))
(define t1.570796 (inexact (/ 1570796 1000000)))
(define t-1.570796 (inexact (/ -1570796 1000000)))
(define t1.47113 (inexact (/ 147113 100000)))
(define t0.1 (inexact (/ 1 10)))
(define t0.0 (inexact 0))
(define t8.0 (inexact 8))
(define t1000.0 (inexact 1000))
(define t0.0996687 (inexact (/ 00996687 10000000)))
(define t2.23607 (inexact (/ 223607 100000)))
;; (define t7.389 (inexact (/ 7389 1000)))
;; (define t1024.0 (inexact 1024))
;; (define t1.570796 (inexact (/ 1570796 1000000)))
;; (define t-1.570796 (inexact (/ -1570796 1000000)))
;; (define t1.47113 (inexact (/ 147113 100000)))
;; (define t0.1 (inexact (/ 1 10)))
;; (define t8.0 (inexact 8))
;; (define t1000.0 (inexact 1000))
;; (define t0.0996687 (inexact (/ 00996687 10000000)))
;; (define t2.23607 (inexact (/ 223607 100000)))

;; (define (almost=? v expected)
;;   (or (fl=? v expected)
;;       (let ([diff (fl* expected (inexact (/ 1 100)))])
;;         (if (zero? diff)
;;             (or (fl<? (fl+ expected diff) v)
;;                 (fl>? (fl+ expected diff) v))
;;         (fl<=? (fl- expected diff) v (fl+ expected diff))))))

;; (display (fl=? (flcos t1.570796) (inexact 0)))
;; (almost=? (flcos t1.570796) (inexact 0))
;; (display (flcos t1.570796) )

;; (format #t "<~a> ~a" (flatan t0.0 t-1.0) (* t1.570796 t2.0))

;
(format #t "~a ~a"  (flatan t0.0 t-1.0) (* t1.570796 t2.0))
