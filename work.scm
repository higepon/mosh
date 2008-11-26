(import (rnrs)
        (clos user)
        (mosh string))

(define-class <point> () x y)

(define-method initialize 'after ((point <point>) init-args)
  (initialize-direct-slots point <point> init-args))

(define-method print-object ((point <point>) port)
  (print-object-with-slots point port))

(define-generic distance-to-origin)

(define-method distance-to-origin ((point <point>))
  (sqrt (+ (expt (slot-ref point 'x) 2)
           (expt (slot-ref point 'y) 2))))

(define p1 (make <point> 'x 3 'y 4))

(format #t "distance of ~a to origin: ~a~%" p1 (distance-to-origin p1))

