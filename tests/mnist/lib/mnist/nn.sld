(define-library (mnist nn)
(import (scheme base) (scheme inexact))
(import (mnist matrix))
(export cross-entropy-error sigmoid sigmoid1 sigmoid-grad softmax)

(begin
;; Other neural network components.
(define (sigmoid1 x)
  (/ 1.0 (+ 1.0 (exp (* -1.0 x)))))

;; Apply sigmoid to matrix
(define (sigmoid a)
  (matrix-map sigmoid1 a))

;; Gradient of sigmoid.
(define (sigmoid-grad a)
  (let ([s (sigmoid a)])
    (matrix-multiply (matrix-sub 1.0 s) s)))

  ;; softmax.
  (define (softmax a)
    (define (sum lst)
      (if (null? lst)
          0
          (+ (car lst) (sum (cdr lst)))))  
    (let* ([c (matrix-max a)]
           [a (matrix-map (lambda (e) (- e c)) a)]
           [mat-exp (matrix-map (lambda (e) (exp e)) a)])
      (vector-map
       (lambda (row)
         (let ([row-sum (sum (vector->list row))])
           (vector-map (lambda (e) (/ e row-sum)) row)))
       mat-exp)))    

;; Cross entropy error.
(define (cross-entropy-error y t)
  (let ([batch-size (matrix-shape y 0)]
        [delta 1e-7])
    (/ (* -1.0 (matrix-sum (matrix-multiply t (matrix-map log (matrix-add y delta))))) batch-size)))
)

)