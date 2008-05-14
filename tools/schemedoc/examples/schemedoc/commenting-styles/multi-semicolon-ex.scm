;;;; .title A Scheme source file with multi-semicolon style documentation comments.  
;;;; .author Kurt Nørmark
;;;; This is a Scheme file with a few simple functions.
;;;; The functions are written and organized with the purpose
;;;; of demonstrating the LAML SchemeDoc tool.
;;;; .laml-resource                 false
;;;; .css-prestylesheet             compact
;;;; .css-stylesheet                fancy
;;;; .css-stylesheet-copying        true
;;;; .keep-syntactical-comment-file false
;;;; .scheme-source-linking         true


;;; The fac and fib functions.
;;; .section-id fib-fac

;; Calculate the factorial of n.
;; .parameter n An integer
;; .pre-condition The integer must be non-negative.
;; .returns n!
(define (fac n)
  (if (= 0 n) 1 (* n (fac (- n 1)))))

;; Calculated the fib function.
;; Notice that this is a very <em>inefficient</em>
;; implementation.
;; .parameter n An integer
;; .pre-condition The integer must be non-negative.
;; .returns The n't fiabonaci number.
(define (fib n)
  (cond ((or (= n 0) (= n 1)) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))


;;; A couple of higher order function.
;;; These functions are useful in many situations.
;;; .section-id higher-order-fn

;; A higher order functions which negates the predicate p. 
;; Negate accepts a predicate and returns the negated predicate.
;; .parameter p a predicate - p: type -> boolean for any type.
;; .returns A predicate that returns the negated value. Thus (not ((negate p) x)) = (p x) for all x.
(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

;; A higher order function that composes two functions.
;; Returns a function which applies f on g.
;; Both f and g are supposed to take a single argument.
;; .parameter f A function of a single parameter.
;; .parameter g A function of a singe parameter.
(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; List selector functions.
;;; The functions in this category are alternatives for car, cadr, etc.
;;; .section-id list-selectors

;; Return the first element of a list
;; .form (first lst)
;; .parameter lst A list
;; .returns the first element of the list
(define first car)

;; Return the second element of a list
;; .form (second lst)
;; .parameter lst A list
;; .returns the second element of the list
(define second cadr)


