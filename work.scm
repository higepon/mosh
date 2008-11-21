;; (define (lcm2 a b)
;;   (/ (* a b) (gcd2 a b)))

;; (define (lcm . n*)
;;   (unless (for-all integer-valued? n*)
;;     (assertion-violation 'lcm "integer valued numbers required"))
;;   (case (length n*)
;;     [(0) 1]
;;     [(1) (abs (first n*))]
;;     [(2) (lcm2 (first n*) (second n*))]
;;     [else
;;      (apply lcm (lcm2 (first n*) (second n*)) (cddr n*))]))


;; (display (lcm 3 2 4.0))
;; (import (rnrs))
;; (display (log -1.000000-0.000000i))

(import (rnrs))
(display 0/0)
