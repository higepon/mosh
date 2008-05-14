;;;; .author Kurt Nørmark
;;;; .title The program prog4
;;;; .affiliation Department of Computer Science, Aalborg University
;;;; This is a demo manual, written to show additional aspects of SchemeDoc.
;;;; These are use of .section-id, cross references among sections, and use of
;;;; the escape character $$.
;;;; .source-destination-delta      man/

;;; The fac and fib functions.
;;; These functions are found in the source file prog4.scm. 
;;; The manual stuff for these is done via SchemeDoc.
;;; See also <a href="#def"> the other section</a>.
;;; .section-id abc

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
;;; These functions are useful in many situations. You may also 
;;; consult <a href="#abc"> the first section</a>.
;;; .section-id def

;; A higher order functions which negates the predicate p. 
;; Negate accepts a predicate and returns the negated predicate.<br>
;; $.parameter In case I need to start this line with a dot, which
;; is not an internal tag.
;; .parameter p A predicate
(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

;; A higher order function that composes two functions.
;; Returns a function which applies f on g.
;; Both f and g are supposed to take a single argument.
;; It is possible to use the escaping character $$ by
;; escaping it. Thus, to writte dolar twice, you must write
;; $$$$. 
;; .internal-references "section" "abc"
;; .internal-references "section" "SECTION2"
;; .misc Miscelaneous information
;; .comment Internal comment
;; .returns Return value description

(define (compose f g)
  (lambda (x)
    (f (g x))))
