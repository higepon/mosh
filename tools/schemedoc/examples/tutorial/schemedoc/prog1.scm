;;;; .title The program prog1
;;;; .author Kurt Normark
;;;; This is a Scheme file with a few simple functions.
;;;; The functions are written and organized with the purpose
;;;; of demonstrating the LAML SchemeDoc tool.
;;;; .laml-resource                 true
;;;; .css-prestylesheet             compact
;;;; .css-stylesheet                original
;;;; .css-stylesheet-copying        true
;;;; .keep-syntactical-comment-file false
;;;; .source-destination-delta      man/

;;; The fac and fib functions.
;;; .section-id fac-fib

;; Calculate the factorial of n
(define (fac n)
  (if (= 0 n) 1 (* n (fac (- n 1)))))

;; Calculated the fib function.
;; Notice that this is a very inefficient 
;; Implementation.
(define (fib n)
  (cond ((or (= n 0) (= n 1)) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))


;;; A couple of higher order function.
;;; These functions are useful in many situations.
;;; .section-id higher-order-fu

;; A higher order functions which negates the predicate p. 
;; Negate accepts a predicate and returns the negated predicate.
(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

;; A higher order function that composes two functions.
;; Returns a function which applies f on g.
;; Both f and g are supposed to take a single argument.
(define (compose f g)
  (lambda (x)
    (f (g x))))
