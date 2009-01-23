(import (rnrs)
        (clos user)
        (clos core)
        (mosh string)
        (mosh test))

(define-class <person> () name age)

(define person1 (make <person>))

(define-generic get-name)
(define-method get-name ((p <person>))
  (slot-ref p 'name))

(define (ppo obj)
  (print-object obj (current-output-port)))

;; 'name is not set, so '()
(test* (slot-ref person1 'name) '())
(test* (get-name person1) '())

(slot-set! person1 'name 'higepon)
(test* (get-name person1) 'higepon)

(define-method initialize ((p <person>) init-args)
  (initialize-direct-slots p <person> init-args))

(define person2 (make <person> 'age 18 'name 'John))

(test* (get-name person2) 'John)

(define-class <painter> (<person>) pen)

(define painter1 (make <painter> 'name 'Paul 'age 18 'pen 'pencil))

(test* (slot-ref painter1 'name) 'Paul)
(test* (slot-ref painter1 'age) 18)
(test* (slot-ref painter1 'pen) '()) ;; no initializer

;; 'after calls initialize of <painter> after calling initialize of <person>
(define-method initialize 'after ((p <painter>) init-args)
  (initialize-direct-slots p <painter> init-args))

(define painter2 (make <painter> 'name 'Paul 'age 28 'pen 'pencil))

(test* (slot-ref painter2 'name) 'Paul)
(test* (slot-ref painter2 'age) 28)
(test* (slot-ref painter2 'pen) 'pencil)
(test* (get-name painter2) 'Paul)
;(print-object-with-slots painter2 (current-output-port))

(test* (class-of painter2) <painter>)
(test* (class-direct-supers <painter>) (list <person>))
(test* (class-direct-supers <person>) (list <object>))
(test* (class-slots <painter>) '((pen) (name) (age)))
(test* (class-direct-slots <painter>) '((pen)))

(define-generic hello)
(define-method hello ((p <person>))
  (format "Hello I'm ~a." (get-name p)))

(test* (hello person1) "Hello I'm higepon.")
(test* (hello painter2) "Hello I'm Paul.")

(define-method hello ((p <painter>))
  (format "Don't touch me <~a>." (get-name p)))

(test* (hello person1) "Hello I'm higepon.")
(test* (hello painter2) "Don't touch me <Paul>.")

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

(test* (work painter2 'moge) "<(collected moge)>")
(test* (apply work painter2 '(moge)) "<(collected moge)>")


;; (define-class <student> (<person>) credits course-list)

;; (define-class <course> () name room time prof student-list)


;; (define-generic get-name)

;; ;; (define-method get-name 'after ((student <student>))
;; ;;   (slot-ref student 'name))

;; ;; (define-method get-name 'after ((student <person>))
;; ;;   (slot-ref student 'name))


;; (define-method initialize 'after ((person <person>) init-args)
;;   (initialize-direct-slots person <person> init-args))

;; (define p (make <person> 'name 'higepon 'age 3))

;; (test* (slot-ref p 'name) 'higepon)

;(display (get-name p))



;; (define-class <point> () x y)

;; (define-class <point3d> (<point>) x y z)

;; ;; 'after って何？

;; (define-method initialize 'after ((point <point3d>) init-args)
;;   (initialize-direct-slots point <point3d> init-args))

;; (define p (make <point3d> 'x 3 'y 4 'z 5))

;; (print-object p (current-output-port))



;; (define-method print-object ((point <point>) port)
;;   (print-object-with-slots point port))

;; (define-generic distance-to-origin)

;; (define-method distance-to-origin ((point <point>))
;;   (sqrt (+ (expt (slot-ref point 'x) 2)
;;            (expt (slot-ref point 'y) 2))))

;; (define p1 (make <point> 'x 3 'y 4))

;; (format #t "distance of ~a to origin: ~a~%\n" p1 (distance-to-origin p1))
