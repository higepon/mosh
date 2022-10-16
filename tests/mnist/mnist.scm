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
        (mnist matrix)
        (rename (only (system) make-f64array f64array-ref f64array-set! f64array-shape f64array-dot-product)
                (f64array-dot-product matrix-mul) (f64array-shape matrix-shape))
        (mosh test))

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
