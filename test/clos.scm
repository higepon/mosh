(import (rnrs)
        (clos user)
        (clos core)
        (mosh)
        (mosh test))

(define-class <person> () name age)

(define person1 (make <person>))

(define-generic get-name)
(define-method get-name ((p <person>))
  (slot-ref p 'name))

(define (ppo obj)
  (print-object obj (current-output-port)))

;; 'name is not set, so '()
(test-null (slot-ref person1 'name))
(test-null (get-name person1))

(slot-set! person1 'name 'higepon)
(test-eq 'higepon (get-name person1))

(define-method initialize ((p <person>) init-args)
  (initialize-direct-slots p <person> init-args))

(define person2 (make <person> 'age 18 'name 'John))

(test-eq 'John (get-name person2))

(define-class <painter> (<person>) pen)

(define painter1 (make <painter> 'name 'Paul 'age 18 'pen 'pencil))

(test-eq 'Paul (slot-ref painter1 'name))
(test-eq 18 (slot-ref painter1 'age))
(test-null (slot-ref painter1 'pen)) ;; no initializer

;; 'after calls initialize of <painter> after calling initialize of <person>
(define-method initialize 'after ((p <painter>) init-args)
  (initialize-direct-slots p <painter> init-args))

(define painter2 (make <painter> 'name 'Paul 'age 28 'pen 'pencil))

(test-eq (slot-ref painter2 'name) 'Paul)
(test-eq (slot-ref painter2 'age) 28)
(test-eq (slot-ref painter2 'pen) 'pencil)
(test-eq (get-name painter2) 'Paul)
;(print-object-with-slots painter2 (current-output-port))

(test-eq (class-of painter2) <painter>)
(test-equal (class-direct-supers <painter>) (list <person>))
(test-equal (class-direct-supers <person>) (list <object>))
(test-equal (class-slots <painter>) '((pen) (name) (age)))
(test-equal (class-direct-slots <painter>) '((pen)))

(define-generic hello)
(define-method hello ((p <person>))
  (format "Hello I'm ~a." (get-name p)))

(test-equal (hello person1) "Hello I'm higepon.")
(test-equal (hello painter2) "Hello I'm Paul.")

(define-method hello ((p <painter>))
  (format "Don't touch me <~a>." (get-name p)))

(test-equal (hello person1) "Hello I'm higepon.")
(test-equal (hello painter2) "Don't touch me <Paul>.")

;; template method pattern
(define-generic work)
(define-generic collect)
(define-generic show)

(define-method work ((p <person>) something)
  (show p (collect p something)))

(define-method collect ((p <painter>) thing)
  (list 'collected thing))

(define-method show ((p <painter>) lst)
  (format "<~a>" lst))

(test-equal "<(collected moge)>" (work painter2 'moge))
(test-equal "<(collected moge)>" (apply work painter2 '(moge)))

(test-results)
