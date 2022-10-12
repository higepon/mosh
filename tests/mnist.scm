(import (scheme base)
        (scheme file)
        (scheme inexact)
        (scheme write)
        (scheme case-lambda)
        (only (scheme vector) vector-count)
        (only (srfi 1) concatenate)
        (only (srfi 27) random-integer)
        (only (srfi 194) make-normal-generator)
        (only (mosh) format)
        (rename (only (system) make-f64array f64array-ref f64array-set! f64array-shape f64array-dot-product)
                (f64array-dot-product matrix-mul) (f64array-shape matrix-shape))
        (mosh test))

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

;; softmax.
(define (softmax a)
  (let* ([c (matrix-max a)]
         [a (matrix-map (lambda (e) (- e c)) a)]
         [mat-exp (matrix-map (lambda (e) (exp e)) a)]
         [row-sum (matrix-sum mat-exp 1)])
   (matrix-divide mat-exp row-sum)))

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

  ])

;; Create a matrix by horizontally stacking column n-times.
(define (matrix-hstack-col col n)
  (unless (= (matrix-shape col 1) 1)
    (error "matrix-hstack-row only supports (N 1) shape" col n))
  (let ([mat (matrix (matrix-shape col 0) n)])
    (do ((i 0 (+ i 1)))
        ((= i (matrix-shape col 0)) mat)
      (do ((j 0 (+ j 1)))
          ((= j n))
          (mat-at mat i j (mat-at col i 0))))))


;; Utilities.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (for-each-with-index proc lst)
  (do ((i 0 (+ i 1))
       (lst lst (cdr lst)))
      ((null? lst))
    (proc i (car lst))))

;; List of list to vector of vector.
(define (list*->vector* l*)
  (list->vector (map (lambda (l) (list->vector l)) l*)))

;; Get argmax of vector.
(define (vector-argmax v)
  (let loop ([i 0]
             [max -inf.0]
             [max-idx 0])
    (cond
     [(= i (vector-length v))
      max-idx]
     [else
      (if (> (vector-ref v i) max)
          (loop (+ i 1) (vector-ref v i) i)
          (loop (+ i 1) max max-idx))])))

;; Create a nested vector.
(define make-vector*
  (case-lambda
   [(m n value)
    (let ([v (make-vector m)])
      (do ((i 0 (+ i 1)))
          ((= i m) v)
        (vec-at v i (make-vector n value))))]
   [(m n)
    (make-vector* m n 0.0)]))

;; Short version of vector-set!, vector-ref and vector-lengt.
(define-syntax vec-at
  (syntax-rules ()
    [(_ v idx)
     (vector-ref v idx)]
    [(_ v idx value)
     (vector-set! v idx value)]))

(define-syntax vec-len
  (syntax-rules ()
    [(_ v) (vector-length v)]))



;; The matrix-map procedure applies proc element-wise to the elements of the matrix
;; and returns a result matrix of the result.
(define (matrix-map proc a)
  (let ([mat (matrix-zeros-like a)]
        [nrows (matrix-shape a 0)]
        [ncols (matrix-shape a 1)])
    (do ((i 0 (+ i 1)))
        ((= i nrows) mat)
      (do ((j 0 (+ j 1)))
          ((= j ncols))
        (mat-at mat i j (proc (mat-at a i j)))))))

;; Matrix transpose
(define (matrix-transpose a)
  (let* ([nrows (matrix-shape a 0)]
         [ncols (matrix-shape a 1)]
         [mat (matrix ncols nrows)])
    (do ((i 0 (+ i 1)))
        ((= i nrows) mat)
      (do ((j 0 (+ j 1)))
          ((= j ncols))
        (mat-at mat j i (mat-at a i j))))))

;; Create a matrix of zeros with the same shape as a given matrix.
(define (matrix-zeros-like a)
  (matrix-full-like a 0.0))

;; Create a full array with the same shape as a given matrix.
(define (matrix-full-like a value)
  (let* ([nrows (matrix-shape a 0)]
         [ncols (matrix-shape a 1)])
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
#;(define (matrix-mul a b)
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

;; Helper for element-wise operations.
(define (matrix-element-wise op a b)
  (let ([mat (matrix-zeros-like a)]
        [nrows (matrix-shape a 0)]
        [ncols (matrix-shape a 1)])
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
   ;; a=#(1 3) b=#(2 3)
   ;;   =>
   ;; a=#(2 3) b=#(2 3)
   [(and (= (matrix-shape a 0) 1) (not (= (matrix-shape b 0) 1)))
    (values (matrix-vstack-row a (matrix-shape b 0)) b)]
   ;; a=#(2 3) b=#(1 3)
   ;;   =>
   ;; a=#(2 3) b=#(2 3)
   [(and (= (matrix-shape b 0) 1) (not (= (matrix-shape a 0) 1)))
    (values a (matrix-vstack-row b (matrix-shape a 0)))]
   ;; a=#(3 1) b=#(3 2)
   ;;   =>
   ;; a=#(3 2) b=#(3 2)
   [(and (= (matrix-shape a 1) 1) (not (= (matrix-shape b 1) 1)))
    (values (matrix-hstack-col a (matrix-shape b 1)) b)]
   ;; a=#(3 2) b=#(3 1)
   ;;   =>
   ;; a=#(3 2) b=#(3 2)
   [(and (= (matrix-shape b 1) 1) (not (= (matrix-shape a 1) 1)))
    (values a (matrix-hstack-col b (matrix-shape a 1)))]
   [else
    (values a b)]))

;; Multiply arguments element-wise.
(define (matrix-multiply a b)
  (let-values ([(a b) (matrix-stretch a b)])
    (unless (equal? (matrix-shape a) (matrix-shape b))
      (error "matrix-add shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise * a b)))

(define (matrix-add a b)
  (let-values ([(a b) (matrix-stretch a b)])
    (unless (equal? (matrix-shape a) (matrix-shape b))
      (error "matrix-add shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise + a b)))

(define (matrix-sub a b)
  (let-values ([(a b) (matrix-stretch a b)])
    (unless (equal? (matrix-shape a) (matrix-shape b))
      (error "matrix-sub shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise - a b)))

(define (matrix-divide a b)
  (let-values ([(a b) (matrix-stretch a b)])
    (unless (equal? (matrix-shape a) (matrix-shape b))
      (error "matrix-divide shapes don't match" (matrix-shape a) (matrix-shape b)))
    (matrix-element-wise / a b)))


(define matrix-sum
  (case-lambda
   [(a)
    (let* ([lst (matrix->list* a)]
           [lst (concatenate lst)])
      (sum lst))]
   [(a axis)
    (cond
     ((= axis 0)
      (let ([mat (matrix 1 (matrix-shape a 1))]
            [nrows (matrix-shape a 0)]
            [ncols (matrix-shape a 1)])
        (do ((i 0 (+ i 1)))
            ((= i nrows) mat)
        (do ((j 0 (+ j 1)))
            ((= j ncols))
          (mat-at mat 0 j (+ (mat-at mat 0 j) (mat-at a i j)))))))
     ((= axis 1)
      (let ([mat (matrix (matrix-shape a 0) 1)]
            [nrows (matrix-shape a 0)]
            [ncols (matrix-shape a 1)])
        (do ((i 0 (+ i 1)))
            ((= i nrows) mat)
          (do ((j 0 (+ j 1)))
              ((= j ncols))
            (mat-at mat i 0 (+ (mat-at mat i 0) (mat-at a i j)))))))
     (else
      (error "matrix-sum only axis=0 or 1 supported" a axis)))]))

(define (matrix-max a)
  (let* ([lst (matrix->list* a)]
         [lst (concatenate lst)])
    (apply max lst)))

(define (matrix-randn nrows ncols)
  (let ([gen (make-normal-generator)])
    (matrix-map (lambda (x) (gen)) (matrix nrows ncols))))

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

;; Cross entropy error.
(define (cross-entropy-error y t)
  (let ([batch-size (matrix-shape y 0)]
        [delta 1e-7])
    (/ (* -1.0 (matrix-sum (matrix-multiply t (matrix-map log (matrix-add y delta))))) batch-size)))

;; Gradient
(define (numerical-gradient func x)
  (let ([h 1e-4]
        [grad (matrix-zeros-like x)])
    (do ((i 0 (+ i 1)))
        ((= i (matrix-shape x 1)) grad)
      (do ((j 0 (+ j 1)))
          ((= j (matrix-shape x 0)))
        (let* ([tmp (mat-at x j i)]
               [fxh1 (func (mat-at x j i (+ tmp h)))]
               [fxh2 (func (mat-at x j i (- tmp h)))])
          (mat-at grad j i (/ (- fxh1 fxh2) (* 2 h)))
          (mat-at x j i tmp))))))

(define gradient-descent
  (case-lambda
   [(func x lr steps)
    (let loop ([i 0] [x x])
      (if (= i steps)
          x
          (let ([grad (numerical-gradient func x)])
            (loop (+ i 1) (matrix-sub x (matrix-multiply lr grad))))))]
   [(func x)
    (gradient-descent func x 0.01 100)]))

;; Read train/test images
(define (load-images path num-images)
  (call-with-port
    (open-binary-input-file path)
    (lambda (port)
      (let ([image-width 28]
            [image-height 28])
        ;; Skip the header
        (read-bytevector 16 port)
        ;; Read images as bytevector
        (bytevector->matrix (read-bytevector (* num-images image-width image-height) port) num-images)))))

;; Read train/test labels
(define (load-labels path num-labels)
  (call-with-port
    (open-binary-input-file path)
    (lambda (port)
      (read-bytevector 8 port)
       ;; Read labels as bytevector
     (bytevector->matrix (read-bytevector num-labels port) 1))))

(define load-mnist
  (case-lambda
    [(data-dir num-train num-test)
        (values
          (matrix-divide (load-images (string-append data-dir "train-images-idx3-ubyte") num-train) 255.0)
          (one-hot (load-labels (string-append data-dir "train-labels-idx1-ubyte") num-train) 10)
          (matrix-divide (load-images (string-append data-dir "t10k-images-idx3-ubyte") num-test) 255.0)
          (one-hot (load-labels (string-append data-dir "t10k-labels-idx1-ubyte") num-test) 10))]
    [(data-dir)
      (load-mnist data-dir 60000 10000)]))

(define (one-hot a num-class)
   (let ([mat (matrix (matrix-shape a 1) num-class 0.0)])
     (let loop ([i 0])
       (cond
         [(= i (matrix-shape a 1))
           mat]
         [else
           (mat-at mat i (exact (mat-at a 0 i)) 1.0)
           (loop (+ i 1))]))))

(define (random-choice num-data batch-size)
  (map (lambda (_) (random-integer num-data)) (make-list batch-size)))

(define (simple-net)
  (let ([w (matrix-randn 2 3)])
    (define (set-w! new-w) (set! w new-w))
    (define (get-w) w)
    (define (predict x)
      (matrix-mul x w))
    (define (loss x t)
      (let* ([z (predict x)]
             [y (softmax z)])
        (cross-entropy-error y t)))
    (values predict loss get-w set-w!)))

(define (two-layer-net input-size hidden-size output-size)
  (let* ([weight-init-std 0.01]
         [w1 (matrix-multiply weight-init-std (matrix-randn input-size hidden-size))]
         [b1 (matrix 1 hidden-size 0)]
         [w2 (matrix-multiply weight-init-std (matrix-randn hidden-size output-size))]
         [b2 (matrix 1 output-size 0)])
    (define (predict x)
      (let* ([a1 (matrix-add (matrix-mul x w1) b1)]
             [z1 (sigmoid a1)]
             [a2 (matrix-add (matrix-mul z1 w2) b2)]
             [y (softmax a2)])
        y))
    (define (loss x t)
      (let ([y (predict x)])
        (cross-entropy-error y t)))
    (define (accuracy x t)
      (let* ([y (predict x)]
             [y (matrix-argmax y)]
             [t (matrix-argmax t)])
        (/ (inexact (vector-count = y t)) (matrix-shape x 0))))
    (define (update-params x t lr)
        (let-values ([[grad-w1 grad-b1 grad-w2 grad-b2] (gradient2 x t)])
          (set! w1 (matrix-sub w1 (matrix-multiply grad-w1 lr)))
          (set! b1 (matrix-sub b1 (matrix-multiply grad-b1 lr)))
          (set! w2 (matrix-sub w2 (matrix-multiply grad-w2 lr)))
          (set! b2 (matrix-sub b2 (matrix-multiply grad-b2 lr)))))
    (define (gradient2 x t)
      (let* ([a1 (matrix-add (matrix-mul x w1) b1)]
             [z1 (sigmoid a1)]
             [a2 (matrix-add (matrix-mul z1 w2) b2)]
             [y (softmax a2)]
             [batch-size (matrix-shape x 0)]
             [dy (matrix-divide (matrix-sub y t) batch-size)]
             [grad-w2 (matrix-mul (matrix-transpose z1) dy)]
             [grad-b2 (matrix-sum dy 0)]
             [dz1 (matrix-mul dy (matrix-transpose w2))]
             [da1 (matrix-multiply (sigmoid-grad a1) dz1)]
             [grad-w1 (matrix-mul (matrix-transpose x) da1)]
             [grad-b1 (matrix-sum da1 0)])
        (values grad-w1 grad-b1 grad-w2 grad-b2)))
    (define (gradient x t)
      (let ([loss-w (lambda (w) (loss x t))])
      (values (numerical-gradient loss-w w1)
              (numerical-gradient loss-w b1)
              (numerical-gradient loss-w w2)
              (numerical-gradient loss-w b2))))
    (values predict loss accuracy gradient update-params)))

;; Matrix shape.
(test-equal '#(1 2) (matrix-shape (matrix ((1.0 2.0)))))
(test-equal '#(2 2) (matrix-shape (matrix ((1.0 2.0) (3.0 4.0)))))

;; Matrix sum.
(test-equal 10.0 (matrix-sum (matrix ((1.0 2.0) (3.0 4.0)))))
(test-equal (matrix ((4.0 6.0))) (matrix-sum (matrix ((1.0 2.0) (3.0 4.0))) 0))
(test-equal (matrix ((3.0) (7.0))) (matrix-sum (matrix ((1.0 2.0) (3.0 4.0))) 1))

;; Create matrix.
(test-equal (matrix ((1 2 3) (4 5 6)))   (bytevector->matrix #u8(1 2 3 4 5 6) 2))
(test-equal (matrix ((1 2) (3 4) (5 6))) (bytevector->matrix #u8(1 2 3 4 5 6) 3))
(test-equal (matrix ((1 2 3 4 5 6)))     (bytevector->matrix #u8(1 2 3 4 5 6) 1))

(test-equal #(3 2) (matrix-shape (matrix-randn 3 2)))

;; Matrix accessor get.
(test-equal 2.0 (mat-at (matrix ((1.0 2.0) (3.0 4.0))) 0 1))

;; Matrix accessor set.
(let ([m (matrix ((1.0 2.0) (3.0 4.0)))])
  (mat-at m 1 0 5.0)
  (test-equal (matrix ((1.0 2.0) (5.0 4.0))) m))

(test-equal '((1.0 2.0) (3.0 4.0)) (matrix->list* (matrix ((1.0 2.0) (3.0 4.0)))))

;; argmax
(test-equal 3 (vector-argmax #(1 2 5 8 4)))
(test-equal #(1 0 2) (matrix-argmax (matrix ((3 4 0) (9 -1 8.8) (0 2 5)))))

;; slice
(test-equal (matrix ((9 -1 3) (0 2 5)))
            (matrix-slice (matrix ((3 4 0) (9 -1 3) (0 2 5))) '(1 2)))

;; Matrix multiplication.
(let ([a (matrix ((1.0 2.0) (3.0 4.0)))]
      [b (matrix ((5.0 6.0) (7.0 8.0)))])
  (test-equal (matrix ((19.0 22.0) (43.0 50.0)))
              (matrix-mul a b)))

(let ([a (matrix ((1.0 2.0) (3.0 4.0) (5.0 6.0)))]
      [b (matrix ((5.0 6.0) (7.0 8.0)))])
  (test-equal (matrix ((19.0 22.0) (43.0 50.0) (67.0 78.0)))
              (matrix-mul a b)))

(let ([a (matrix ((1.0 2.0 3.0) (4.0 5.0 6.0)))]
      [b (matrix ((1.0) (2.0) (3.0)))])
  (test-equal (matrix ((14.0) (32.0)))
              (matrix-mul a b)))

;; Matrix addition.
(let ([a (matrix ((1.0 2.0) (3.0 4.0)))]
      [b (matrix ((5.0 6.0) (7.0 8.0)))])
  (test-equal (matrix ((6.0 8.0) (10.0 12.0)))
              (matrix-add a b)))

;; Matrix addition broadcast.
(let ([a (matrix ((1.0 2.0) (3.0 4.0)))]
      [b (matrix ((5.0 6.0)))])
  (test-equal (matrix ((6.0 8.0) (8.0 10.0)))
              (matrix-add a b)))

;; Matrix addition broadcast.
(let ([a (matrix ((1.0 2.0) (3.0 4.0)))]
      [b (matrix ((5.0 6.0)))])
  (test-equal (matrix ((6.0 8.0) (8.0 10.0)))
              (matrix-add b a)))

;; Matrix addition broadcast.
(let ([a (matrix ((1.0 2.0) (3.0 4.0)))]
      [b 2])
  (test-equal (matrix ((3.0 4.0) (5.0 6.0)))
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

;; Matrix divide arguments element-wise.
(let ([a (matrix ((1 2) (4 4)))]
      [b (matrix ((1 2) (2 1)))])
  (test-equal (matrix ((1 1) (2 4)))
              (matrix-divide a b)))

;; Matrix stretch.
(test-values (values (matrix ((1 1) (1 1))) (matrix ((1 2) (3 4))))
             (matrix-stretch 1 (matrix ((1 2) (3 4)))))
(test-values (values (matrix ((1 2) (3 4))) (matrix ((1 1) (1 1))))
             (matrix-stretch (matrix ((1 2) (3 4))) 1))

(test-values (values (matrix ((5 6) (5 6))) (matrix ((1 2) (3 4))))
             (matrix-stretch (matrix ((5 6))) (matrix ((1 2) (3 4)))))
(test-values (values  (matrix ((1 2) (3 4))) (matrix ((5 6) (5 6))))
             (matrix-stretch (matrix ((1 2) (3 4))) (matrix ((5 6)))))

(test-values (values (matrix ((1 2 3))) (matrix ((1 1 1))))
             (matrix-stretch (matrix ((1 2 3))) (matrix ((1)))))
(test-values (values (matrix ((1 1 1))) (matrix ((1 2 3))))
             (matrix-stretch (matrix ((1))) (matrix ((1 2 3)))))

;; Matrix transpose.
(test-equal (matrix ((1 2 3) (4 5 6)))
            (matrix-transpose (matrix ((1 4) (2 5) (3 6)))))

;; Matrix stack
(test-equal (matrix ((1 2) (1 2) (1 2))) (matrix-vstack-row (matrix ((1 2))) 3))
(test-equal (matrix ((1 1 1) (2 2 2) (3 3 3))) (matrix-hstack-col (matrix ((1) (2) (3))) 3))

;; matrix-map
(test-equal (matrix ((2 4) (6 8))) (matrix-map (lambda (e) (* e 2)) (matrix ((1 2) (3 4)))))

;; Other neural network components
;;   sigmoid
(test-true (good-enough? 0.57444252 (sigmoid1 0.3)))
(test-true (let ([mat (sigmoid (matrix ((1.0 2.0))))])
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

;;   cross-entropy-error
(test-true (let ([t (matrix ((0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))]
                 [y (matrix ((0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0)))])
             (good-enough? 0.510825457099338 (cross-entropy-error y t))))

(test-true (let ([t (matrix ((0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))]
                 [y (matrix ((0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0)))])
             (good-enough? 2.9957302735559908 (cross-entropy-error y t))))

(test-true (let ([t (matrix ((0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))]
                 [y (matrix ((0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0) (0.1 0.05 0.6 0.0 0.05 0.1 0.0 0.1 0.0 0.0)))])
             (good-enough? 1.7532778653276644 (cross-entropy-error y t))))

;;   numerical-gradient
(let ([mat (numerical-gradient
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((3.0 4.0))))])
  (test-true (good-enough?  6.0 (mat-at mat 0 0)))
  (test-true (good-enough?  8.0 (mat-at mat 0 1))))

;;  gradient-descent
(let ([mat (gradient-descent
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((-3.0 4.0))) 1e-10 100)])
  (test-true (good-enough? -2.999 (mat-at mat 0 0)))
  (test-true (good-enough? 3.999 (mat-at mat 0 1))))

(let-values (([x-train t-train x-test t-test] (load-mnist "/workspace/" 60 8)))
  (test-equal #(60 784) (matrix-shape x-train))
  (test-equal #(60 10) (matrix-shape t-train))
  (test-equal #(8 784) (matrix-shape x-test))
  (test-equal #(8 10) (matrix-shape t-test))
  (let* ([batch-idx* (random-choice 60 4)]
         [batch (matrix-slice x-train batch-idx*)])
    (test-equal #(4 784) (matrix-shape batch)))
)

(let-values (([predict loss get-w set-w!] (simple-net)))
  (set-w! (matrix ((0.47355232 0.9977393 0.84668094)
                   (0.85557411 0.03563661 0.69422093))))
  (let* ([x (matrix ((0.6 0.9)))]
         [t (matrix ((0 0 1)))]
         [pred (predict x)])
    (test-true (good-enough? 1.054148091 (mat-at pred 0 0)))
    (test-true (good-enough? 0.63071653 (mat-at pred 0 1)))
    (test-true (good-enough? 1.1328074 (mat-at pred 0 2)))
    (test-equal #(2) (matrix-argmax pred))
    (test-true (good-enough? 0.9280682857864075 (loss x t)))
    (test-equal #(2 3) (matrix-shape (numerical-gradient (lambda (w) (loss x t)) (get-w))))))


(let-values (([predict loss accuracy gradient update-params] (two-layer-net 3 10 3)))
  (test-equal #(2 3) (matrix-shape (predict (matrix-randn 2 3))))
  (loss (matrix-randn 2 3) (matrix ((0 0 1) (0 1 0))))
  (accuracy (matrix-randn 2 3) (matrix ((0 0 1) (0 1 0))))
  (gradient (matrix-randn 2 3)  (matrix ((0 0 1) (0 1 0)))))

(test-results)

;; Hyper parameters.
(define train-size 60000)
(define batch-size 100)
(define num-classes 10)
(define hidden-size 50)
(define lr 0.1)
(define epochs 8)
(define iteration-per-epoch (/ train-size batch-size))

(let-values (([x-train t-train x-test t-test] (load-mnist "/workspace/" train-size num-classes)))
  (let-values (([predict loss accuracy gradient update-params] (two-layer-net 784 hidden-size num-classes)))
    (do ((epoch 0 (+ epoch 1)))
        ((= epoch epochs))
      (display "train accuracy=")
      (display (accuracy x-train t-train))      
      (newline)        
      (display "test accuracy=")
      (display (accuracy x-test t-test))
      (newline)

      (do ((i 0 (+ i 1)))
          ((= i iteration-per-epoch))
        (let* ([batch-idx* (random-choice train-size batch-size)]
               [x-batch (matrix-slice x-train batch-idx*)]
               [t-batch (matrix-slice t-train batch-idx*)])
          (update-params x-batch t-batch lr)
          (when (= 0 (remainder i 100))
            (display "loss= ")
            (display (loss x-batch t-batch))
            (newline)))))))
