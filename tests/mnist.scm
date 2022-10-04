(import (scheme base)
        (scheme file)
        (scheme inexact)
        (scheme write)
        (scheme case-lambda)
        (only (srfi 1) concatenate)
        (only (mosh) format)
        (mosh test))

;; Utilities.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;; List of list to vector of vector.
(define (list*->vector* l*)
    (list->vector (map (lambda (l) (list->vector l)) l*)))

;; Create a nested vector.
(define make-vector*
    (case-lambda
      [(m n value)
        (let ([v (make-vector m)])
          (do ((i 0 (+ i 1)))
              ((= i m) v)
            (vec-at v i (make-vector n value))))]
      [(m n)
        (make-vector* m n 0)]))

;; Short version of vector-set! and vector-ref.
(define-syntax vec-at
    (syntax-rules ()
      [(_ v idx)
        (vector-ref v idx)]
      [(_ v idx value)
        (vector-set! v idx value)]))

(define-syntax vec-len
    (syntax-rules ()
      [(_ v) (vector-length v)]))

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
        (vec-at (vec-at m i) j value)]))

;; The matrix-map procedure applies proc element-wise to the elements of the matrix
;; and returns a result matrix of the result.
(define (matrix-map proc a)
    (let ([mat (matrix-zeros-like a)]
          [nrows (vec-at (matrix-shape a) 0)]
          [ncols (vec-at (matrix-shape a) 1)])
      (do ((i 0 (+ i 1)))
          ((= i nrows) mat)
          (do ((j 0 (+ j 1)))
              ((= j ncols))
              (mat-at mat i j (proc (mat-at a i j)))))))

;; Convert matrix to nested list.
(define (matrix->list* a)
    (map vector->list (vector->list a)))

;; Matrix shape.
;; N.B For now we only support 2D matrix.
(define (matrix-shape x)
    `#(,(vec-len x) ,(vec-len (vec-at x 0))))

;; Create a matrix of zeros with the same shape as a given matrix.
(define (matrix-zeros-like a)
    (let* ([nrows (vec-at (matrix-shape a) 0)]
           [ncols (vec-at (matrix-shape a) 1)])
        (matrix nrows ncols 0)))

;; Create a matrix from bytevector.
(define (bytevector->matrix bv nrows)
    (let* ([ncols (/ (bytevector-length bv) nrows)]
           [mat (matrix nrows ncols)])
      (do ((i 0 (+ i 1)))
          ((= i nrows) mat)
            (do ((j 0 (+ j 1)))
              ((= j ncols))
              (mat-at mat i j (bytevector-u8-ref bv (+ j (* ncols i))))))))

;; Matrix multiplication.
(define (matrix-mul a b)
    (unless (= (vec-at (matrix-shape a) 1) (vec-at (matrix-shape b) 0))
        (error "matrix-mul shapes don't match" (matrix-shape a) (matrix-shape b)))
    (let* ([nrows (vec-at (matrix-shape a) 0)]
           [ncols (vec-at (matrix-shape b) 1)]
           [m     (vec-at (matrix-shape a) 1)]
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

(define (matrix-add a b)
    (unless (equal? (matrix-shape a) (matrix-shape b))
        (error "matrix-add shapes don't match" (matrix-shape a) (matrix-shape b)))
    (let ([mat (matrix-zeros-like a)]
          [nrows (vec-at (matrix-shape a) 0)]
          [ncols (vec-at (matrix-shape a) 1)])
      (do ((i 0 (+ i 1)))
          ((= i nrows) mat)
            (do ((j 0 (+ j 1)))
              ((= j ncols))
              (mat-at mat i j (+ (mat-at a i j) (mat-at b i j)))))))

(define (matrix-sum a)
    (let* ([lst (matrix->list* a)]
           [lst (concatenate lst)])
      (sum lst)))

(define (matrix-max a)
    (let* ([lst (matrix->list* a)]
           [lst (concatenate lst)])
      (apply max lst)))

;; Other neural network components.
(define (sigmoid1 x)
    (/ 1 (+ 1 (exp (* -1 x)))))

;; Apply sigmoid to matrix
(define (sigmoid a)
    (matrix-map sigmoid1 a))

;; softmax.
(define (softmax a)
    (unless (= 1 (vec-at (matrix-shape a) 0))
        (error "softmax requires shape of (1 N)" (matrix-shape a)))
    (let* ([c (matrix-max a)]
           [a (matrix-map (lambda (e) (- e c)) a)]
           [mat-exp (matrix-map (lambda (e) (exp e)) a)]
           [sum-exp (matrix-sum  mat-exp)])
      (matrix-map (lambda (e) (/ e sum-exp)) mat-exp)))

;; Matrix shape.
(test-equal '#(1 2) (matrix-shape (matrix ((1 2)))))
(test-equal '#(2 2) (matrix-shape (matrix ((1 2) (3 4)))))

;; Create matrix.
(test-equal (matrix ((1 2 3) (4 5 6)))   (bytevector->matrix #u8(1 2 3 4 5 6) 2))
(test-equal (matrix ((1 2) (3 4) (5 6))) (bytevector->matrix #u8(1 2 3 4 5 6) 3))
(test-equal (matrix ((1 2 3 4 5 6)))     (bytevector->matrix #u8(1 2 3 4 5 6) 1))

;; Matrix accessor get.
(test-equal 2 (mat-at (matrix ((1 2) (3 4))) 0 1))

;; Matrix accessor set.
(let ([m (matrix ((1 2) (3 4)))])
    (mat-at m 1 0 5)
    (test-equal #(#(1 2) #(5 4)) m))

(test-equal '((1 2) (3 4)) (matrix->list* (matrix ((1 2) (3 4)))))

;; Matrix multiplication.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((5 6) (7 8)))])
   (test-equal (matrix ((19 22) (43 50)))
               (matrix-mul a b)))

(let ([a (matrix ((1 2) (3 4) (5 6)))]
      [b (matrix ((5 6) (7 8)))])
   (test-equal (matrix ((19 22) (43 50) (67 78)))
               (matrix-mul a b)))

(let ([a (matrix ((1 2 3) (4 5 6)))]
      [b (matrix ((1) (2) (3)))])
   (test-equal (matrix ((14) (32)))
               (matrix-mul a b)))

;; Matrix additions.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((5 6) (7 8)))])
   (test-equal (matrix ((6 8) (10 12)))
               (matrix-add a b)))

;; matrix-map
(test-equal (matrix ((2 4) (6 8))) (matrix-map (lambda (e) (* e 2)) (matrix ((1 2) (3 4)))))

;; Other neural network components
;;   sigmoid
(test-true (good-enough? 0.57444252 (sigmoid1 0.3)))
(test-true (let ([mat (sigmoid (matrix ((1 2))))])
             (and (good-enough? 0.7310585 (mat-at mat 0 0))
                  (good-enough? 0.8807970 (mat-at mat 0 1)))))

;;   softmax
(test-true (let ([mat (softmax (matrix ((0.3 2.9 4.0))))])
             (and (good-enough? 0.01821127 (mat-at mat 0 0))
                  (good-enough? 0.24519181 (mat-at mat 0 1))
                  (good-enough? 0.73659691 (mat-at mat 0 2)))))

;; Read training images
(define (load-train)
  (call-with-port (open-binary-input-file "/workspace/train-images-idx3-ubyte")
    (lambda (port)
      (let ([num-images 60000]
            [image-width 28]
            [image-height 28])
        ;; Skip the header
        (read-bytevector 16 port)
        ;; Read images as bytevector
        (bytevector->matrix (read-bytevector (* num-images image-width image-height) port) num-images)))))

(test-equal #(60000 784) (matrix-shape (load-train)))

(test-results)
