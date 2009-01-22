(import (rnrs)
        (clos user)
        (clos core)
        (mosh string)
        (mosh test))

(define-class <person> () name age)


(define-class <student> (<person>) credits course-list)

(define-class <course> () name room time prof student-list)


(define-generic get-name)

;; (define-method get-name 'after ((student <student>))
;;   (slot-ref student 'name))

;; (define-method get-name 'after ((student <person>))
;;   (slot-ref student 'name))


(define-method initialize 'after ((person <person>) init-args)
  (initialize-direct-slots person <person> init-args))

(define p (make <person> 'name 'higepon 'age 3))

(test* (slot-ref p 'name) 'higepon)

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
