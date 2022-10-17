;; Header here.
(define-library (mnist matrix)
  (export bytevector->matrix matrix-max matrix matrix-shape matrix-mul matrix-hstack-col matrix-add matrix-argmax mat-at matrix-divide matrix-map matrix-multiply matrix-randn matrix-slice matrix-stretch matrix-sub matrix-sum matrix-transpose matrix-vstack-row matrix-zeros-like matrix->list* vector-argmax one-hot)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme inexact))
  (import (only (srfi 1) concatenate))
  (import (only (srfi 194) make-normal-generator))
  (import (rename (only (system) make-f64array f64array-ref f64array-set! f64array-shape f64array-dot-product)
                (f64array-dot-product matrix-mul) (f64array-shape matrix-shape))
)


(cond-expand
  [mosh

(define (vector*->matrix v)
  (let* ([nrows (vec-len v)]
         [ncols (vec-len (vec-at v 0))]
         [mat (matrix nrows ncols)])
    (do ((i 0 (+ i 1)))
        ((= i nrows) mat)
      (do ((j 0 (+ j 1)))
          ((= j ncols))
          (mat-at mat i j (vec-at (vec-at v i) j))))))

;; Create matrix.
(define-syntax matrix
  (syntax-rules ()
    [(_ array)
      (vector*->matrix (list*->vector* 'array))]
    [(_ m n)
     (make-f64array m n)]
    [(_ m n value)
     (make-f64array m n value)]))

;; Get or set (i, j) element of matrix.
(define-syntax mat-at
  (syntax-rules ()
    [(_ m i j)
     (f64array-ref m i j)]
    [(_ m i j value)
     (begin (f64array-set! m value i j) m)]))

;; Convert matrix to nested list.
(define (matrix->list* a)
  (define (row->list row)
    (let loop ([ret '()]
               [i 0])
       (if (= i (matrix-shape a 1))
           (reverse ret)
           (loop (cons (mat-at a row i) ret) (+ i 1)))))
  (let loop ([ret '()]
             [row 0])
    (if (= row (matrix-shape a 0))
        (reverse ret)
        (loop (cons (row->list row) ret) (+ row 1)))))

;; Create a matrix by vertically stacking row n-times.
(define (matrix-vstack-row row n)
  (unless (= (matrix-shape row 0) 1)
    (error "matrix-vstack-row only supports (1 N) shape" row n))
  (let ([mat (matrix n (matrix-shape row 1))])
    (do ((i 0 (+ i 1)))
        ((= i n) mat)
      (do ((j 0 (+ j 1)))
          ((= j (matrix-shape row 1)))
          (mat-at mat i j (mat-at row 0 j))))))

;; argmax
#;(define (matrix-argmax a)
  (let* ([nrows (matrix-shape a 0)]
         [ncols (matrix-shape a 1)]
         [ret (make-vector nrows)])
    (define (row-argmax row)
      (let loop ([col 0]
                 [max-val -inf.0]
                 [max-idx -1])
        (if (= col ncols)
            max-idx
            (if (> (mat-at a row col) max-val)
                (loop (+ col 1) (mat-at a row col) col)
                (loop (+ col 1) max-val max-idx)))))
   (do ([i 0 (+ i 1)])
       [(= i nrows) ret]
      (vec-at ret i (row-argmax i)))))
(define (matrix-argmax a)      
  (vector-map vector-argmax (list*->vector* (matrix->list* a))))

;; Slice
(define (matrix-slice a row-index*)
  (let ([mat (matrix (length row-index*) (matrix-shape a 1))])
    (for-each-with-index
      (lambda (i row-index)
        (do ((j 0 (+ j 1)))
            ((= j (matrix-shape a 1)))
          (mat-at mat i j (mat-at a row-index j))))
      row-index*)
    mat))



  ]
  [else

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
     (vec-at (vec-at m i) j)]
    [(_ m i j value)
     (begin (vec-at (vec-at m i) j value) m)]))

;; Matrix shape.
;; N.B For now we only support 2D matrix.
#;(define matrix-shape
  (case-lambda
   [(a) `#(,(vec-len a) ,(vec-len (vec-at a 0)))]
   [(a n)
    (if (= n 0)
        (vec-len a)
        (vec-len (vec-at a 0)))]))

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
      (vec-at mat i (vector-copy (vec-at row 0))))))

;; argmax
(define (matrix-argmax a)
  (vector-map vector-argmax a))

;; Slice
;; TODO: Should this return copy?
(define (matrix-slice a row-index*)
  (let ([mat (matrix (length row-index*) (matrix-shape a 1))])
    (for-each-with-index
      (lambda (i row-index)
        (vec-at mat i (vec-at a row-index)))
      row-index*)
    mat))



  ])

(include "matrix-impl.scm")

)