;; Header here.
(define-library (mnist matrix)
  (export softmax bytevector->matrix matrix matrix-shape matrix-mul matrix-hstack-col matrix-add matrix-argmax mat-at matrix-divide matrix-map matrix-multiply matrix-randn matrix-slice matrix-stretch matrix-sub matrix-sum matrix-transpose matrix-vstack-row matrix-zeros-like matrix->list* vector-argmax one-hot vec-len vec-at)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme inexact))
  (import (only (srfi 1) concatenate))
  (cond-expand
   [gauche
      (import (rename (data random) (reals-normal$ make-normal-generator)))]
   [else
     (import (only (srfi 194) make-normal-generator))])

(begin
;; Create matrix.
;; We use nested list as intenal representation.
(define-syntax matrix
  (syntax-rules ()
    [(_ array)
     (list*->vector* 'array)]
    [(_ m n)
     (make-vector* m n)]
    [(_ m n value)
     (make-vector* m n value)]))

;; Get or set (i, j) element of matrix.
(define-syntax mat-at
  (syntax-rules ()
    [(_ m i j)
     (vector-ref (vector-ref m i) j)]
    [(_ m i j value)
     (begin (vector-set! (vector-ref m i) j value) m)]))

;; Matrix multiplication.
(define (matrix-mul a b)
  (unless (= (matrix-shape a 1) (matrix-shape b 0))
    (error "matrix-mul shapes don't match" (matrix-shape a) (matrix-shape b)))
  (let* ([nrows (matrix-shape a 0)]
         [ncols (matrix-shape b 1)]
         [m     (matrix-shape a 1)]
         [mat (matrix nrows ncols)])
    (define (mul row col)
      (let loop ([k 0]
                 [ret 0])
        (if (= k m)
            ret
            (loop (+ k 1) (+ ret (* (mat-at a row k) (mat-at b k col)))))))
    (do ((i 0 (+ i 1)))
        ((= i nrows) mat)
      (do ((j 0 (+ j 1)))
          ((= j ncols))
        (mat-at mat i j (mul i j))))))     

;; Matrix shape.
;; N.B For now we only support 2D matrix.
(define matrix-shape
  (case-lambda
   [(a) `#(,(vector-length a) ,(vector-length (vector-ref a 0)))]
   [(a n)
    (if (= n 0)
        (vector-length a)
        (vector-length (vector-ref a 0)))]))

;; Convert matrix to nested list.
(define (matrix->list* a)
  (map vector->list (vector->list a)))

;; Create a matrix by vertically stacking row n-times.
(define (matrix-vstack-row row n)
  (unless (= (matrix-shape row 0) 1)
    (error "matrix-vstac-row only supports (1 N) shape" row n))
  (let ([mat (matrix n (matrix-shape row 1))])
    (do ((i 0 (+ i 1)))
        ((= i n) mat)
      (vector-set! mat i (vector-copy (vector-ref row 0))))))

;; argmax
(define (matrix-argmax a)
  (vector-map vector-argmax a))

;; Slice
;; TODO: Should this return copy?
(define (matrix-slice a row-index*)
  (let ([mat (matrix (length row-index*) (matrix-shape a 1))])
    (for-each-with-index
      (lambda (i row-index)
        (vector-set! mat i (vector-ref a row-index)))
      row-index*)
    mat))

;; softmax.
(define (softmax a)
  (let* ([c (matrix-max a)]
         [a (matrix-map (lambda (e) (- e c)) a)]
         [mat-exp (matrix-map (lambda (e) (exp e)) a)])
    (vector-map
     (lambda (row)
       (let ([row-sum (sum (vector->list row))])
         (vector-map (lambda (e) (/ e row-sum)) row)))
     mat-exp)))



)


(include "matrix-impl.scm")
)