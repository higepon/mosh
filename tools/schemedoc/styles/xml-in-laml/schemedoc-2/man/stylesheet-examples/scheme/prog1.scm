;;;; .title SchemeDoc Demo
;;;; .author Kurt Normark
;;;; .affiliation Aalborg University, Denmark
;;;; This is a brief example of a Scheme program with multi-semicolon SchemeDoc comments.
;;;; We have used this example in the paper "Scheme Documentation tools". Here the example
;;;; is used to illustrate the use of different CSS stylesheets.

;;; Factorials. 
;;; .section-id fac-stuff
;;; This section demonstrates a plain and well-known function, namely the factorial function.

;; The factorial function. Also known as n! The factorial function multiplies the numbers from 1 to n.
;; .parameter n An integer
;; .pre-condition n >= 0
;; .returns n * (n-1) * ... * 1
(define (fac n)
 (if (= n 0) 1 (* n (fac (- n 1)))))

;;; List selection functions.
;;; .section-id list-stuff
;;; This section demonstrates two aliased functions.

;; An alias of car. 
;; .returns The first component of a cons cell
;; .form (head pair)
;; .parameter pair A cons cell
(define head car)

;; An alias of cdr.
;; .returns The second component of a cons cell
;; .form (tail pair)
;; .parameter pair A cons cell
(define tail cdr)
