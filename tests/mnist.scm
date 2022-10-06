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
   (matrix-full-like a 0))

;; Create a full array with the same shape as a given matrxi.
(define (matrix-full-like a value)
    (let* ([nrows (vec-at (matrix-shape a) 0)]
           [ncols (vec-at (matrix-shape a) 1)])
        (matrix nrows ncols value)))

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

;; Multiply arguments element-wise.
(define (matrix-multiply a b)
    (unless (equal? (matrix-shape a) (matrix-shape b))
        (error "matrix-multiply shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise * a b))

;; Helper for element-wise operations.
(define (matrix-element-wise op a b)
    (let ([mat (matrix-zeros-like a)]
        [nrows (vec-at (matrix-shape a) 0)]
        [ncols (vec-at (matrix-shape a) 1)])
        (do ((i 0 (+ i 1)))
            ((= i nrows) mat)
            (do ((j 0 (+ j 1)))
                ((= j ncols))
                (mat-at mat i j (op (mat-at a i j) (mat-at b i j)))))))

;; Stretch matrix if necessary.
;; This is used for broadcasting.
(define (matrix-stretch a b)
  (cond
    ;; scalar to matrix
    [(number? a)
      (values (matrix-full-like b a) b)]
    [(number? b)
      (values a (matrix-full-like a b))]
    ;; a's shape is (1 ncols-a). We can stretch to (nrows-b ncols-a)
    [(= (vec-at (matrix-shape a) 0) 1)
      (values (matrix-vstack-row a (vec-at (matrix-shape b) 0)) b)]
    ;; b's shape is (1 ncols-b). We can stretch to (nrows-a ncols-b)
    [(= (vec-at (matrix-shape b) 0) 1)
      (values a (matrix-vstack-row b (vec-at (matrix-shape a) 0)))]
    [else
      (values a b)]))

;; Create a matrix by vertically stacking row n-times.
(define (matrix-vstack-row row n)
    (unless (= (vec-at (matrix-shape row) 0) 1)
        (error "matrix-vstac-row only supports (1 N) shape" row n))
    (let ([mat (matrix n (vec-at (matrix-shape row) 1))])
      (do ((i 0 (+ i 1)))
          ((= i n) mat)
          (vec-at mat i (vector-copy (vec-at row 0))))))

(define (matrix-add a b)
  (let-values ([(a b) (matrix-stretch a b)])
    (unless (equal? (matrix-shape a) (matrix-shape b))
      (error "matrix-add shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise + a b)))

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

;; Cross entropy error.
(define (cross-entropy-error y t)
  (let ([delta 1e-7])
    (* -1 (matrix-sum (matrix-multiply t (matrix-map log (matrix-add y delta)))))))

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

;; Matrix addition.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((5 6) (7 8)))])
   (test-equal (matrix ((6 8) (10 12)))
               (matrix-add a b)))

;; Matrix addition broadcast.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((5 6)))])
   (test-equal (matrix ((6 8) (8 10)))
               (matrix-add a b)))

;; Matrix addition broadcast.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((5 6)))])
   (test-equal (matrix ((6 8) (8 10)))
               (matrix-add b a)))

;; Matrix addition broadcast.
(let ([a (matrix ((1 2) (3 4)))]
      [b 2])
   (test-equal (matrix ((3 4) (5 6)))
               (matrix-add a b)))

;; Matrix addition broadcast.
(let ([a (matrix ((1 2) (3 4)))]
      [b 2])
   (test-equal (matrix ((3 4) (5 6)))
               (matrix-add b a)))

;; Matrix multiply arguments element-wise.
(let ([a (matrix ((1 2) (3 4)))]
      [b (matrix ((0 2) (3 1)))])
   (test-equal (matrix ((0 4) (9 4)))
               (matrix-multiply a b)))

;; Matrix vstack
(test-equal (matrix ((1 2) (1 2) (1 2))) (matrix-vstack-row (matrix ((1 2))) 3))

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

(test-true (let ([mat (softmax (matrix ((0.3 2.9 4.0) (0.3 2.9 4.0))))])
             (and (good-enough? 0.01821127 (mat-at mat 0 0))
                  (good-enough? 0.24519181 (mat-at mat 0 1))
                  (good-enough? 0.73659691 (mat-at mat 0 2))
                  (good-enough? 0.01821127 (mat-at mat 1 0))
                  (good-enough? 0.24519181 (mat-at mat 1 1))
                  (good-enough? 0.73659691 (mat-at mat 1 2)))))

(test-true (let ([t (matrix ((0) (0) (1) (0) (0) (0) (0) (0) (0) (0)))]
                 [y (matrix ((0.1) (0.05) (0.6) (0.0) (0.05) (0.1) (0.0) (0.1) (0.0) (0.0)))])
                  (good-enough? 0.510825457099338 (cross-entropy-error y t))))


;; Read training images
(define (load-train)
  (call-with-port (open-binary-input-file "/workspace/train-images-idx3-ubyte")
    (lambda (port)
      (let ([num-images 6] ;; TODO 60000
            [image-width 28]
            [image-height 28])
        ;; Skip the header
        (read-bytevector 16 port)
        ;; Read images as bytevector
        (bytevector->matrix (read-bytevector (* num-images image-width image-height) port) num-images)))))

(test-equal #(6 784) (matrix-shape (load-train)))

(define w1 (matrix 784 50))
(define b1 (matrix 1 50))
(define w2 (matrix 50 100))
(define b2 (matrix 1 100))
(define w3 (matrix 100 10))
(define b3 (matrix 1 10))

(let* ([x (load-train)]
       [a1 (matrix-add (matrix-mul x w1) b1)]
       [z1 (sigmoid a1)]
       [a2 (matrix-add (matrix-mul z1 w2) b2)]
       [z2 (sigmoid a2)]
       [a3 (matrix-add (matrix-mul z2 w3) b3)]
       [y (softmax a3)])
  (display y))

(test-results)

;; todo matrix-nrows matrix-ncols