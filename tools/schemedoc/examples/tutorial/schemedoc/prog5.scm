;;;; This is a demo manual, written to show additional aspects of SchemeDoc.
;;;; In particular Scheme source linking.

;;; Mutatator and Selector functions.

;; Returns a function, which selects element number n in a list.
;; The second parameter, which is optional, is used for error message purposes.
;; In general, this parameter should be a string corresponding to the name of the selector function.
;; If the second parameter is given, we check whether the list is long enough for selection.
;; If not, we give a decent error message. We recommend use of the second parameter in order to
;; avoid meaningless error messages.
;; The first element is number 1.
;; (make-selector-function 1) corresponds to car, (make-selector-function 2) corresponds to cadr, etc.
;; .form (make-selector-function n [selector-name])
(define (make-selector-function n . optional-parameter-list)
 (let ((selector-name (optional-parameter 1 optional-parameter-list #f)))
   (if selector-name
       (lambda (lst) 
         (cond ((list? lst)
                 (let ((lgt (length lst)))
		   (if (> n lgt)
		       (display-error (string-append "The selector function " (as-string selector-name) ": " 
						     "The list "  (as-string lst) " is is too short for selection. "
						     "It must have at least " (as-string n) " elements."
						     ))
		       (list-ref lst (- n 1)))))
               (else (display-error (string-append "The selector function " (as-string selector-name) ": " 
						   "The parameter  "  (as-string lst) "  is supposed to be a list. "
                                                   "In addition, it must have at least "
				                    (as-string n) " elements."
						     )))))
       (lambda (lst) (list-ref lst (- n 1))))))

;; Make and return a mutator function which mutates element number n in a list.
;; The returned function takes a list and a new value as arguments.
;; This function takes one optional parameter, which is the name of the mutator
;; This is used for error message purposes.
(define (make-mutator-function n . optional-parameter-list)
 (let ((mutator-name (optional-parameter 1 optional-parameter-list)))
   (if mutator-name
       (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "The mutator function " (as-string mutator-name) ": " 
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."
                               ))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value)))))
      (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "Error in mutator:"
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value))))))))

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
;; .internal-references "section" "abc"
;; .internal-references "section" "SECTION2"
;; .misc Miscelaneous information
;; .comment Internal comment
;; .returns Return value description

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Another function

;; An alias of as-string from the general LAML library.
(define my-as-string as-string)