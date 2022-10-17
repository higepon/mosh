(import (scheme base)
        (scheme complex)
        (scheme file)
        (scheme inexact)
        (scheme write)
        (scheme case-lambda)
        (only (scheme vector) vector-count)
        (only (srfi 27) random-integer)
        (mnist matrix)
        (mnist nn)
        (srfi 64))

;; Test helper.
(define (good-enough? x y)
    ;; relative error should be with 0.1%, but greater
    ;; relative error is allowed when the expected value
    ;; is near zero.
    (cond ((not (number? x)) #f)
          ((not (number? y)) #f)
          ((or (not (real? x))
               (not (real? y)))
           (and (good-enough? (real-part x) (real-part y))
                (good-enough? (imag-part x) (imag-part y))))
          ((infinite? x)
           (= x (* 2.0 y)))
          ((infinite? y)
           (= (* 2.0 x) y))
          ((nan? y)
           (nan? x))
          ((> (magnitude y) 1e-6)
           (< (/ (magnitude (- x y))
                 (magnitude y))
              1e-3))
          (else
           (< (magnitude (- x y)) 1e-6))))

(test-begin "MNIST")

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






;;   numerical-gradient
(let ([mat (numerical-gradient
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((3.0 4.0))))])
  (test-assert (good-enough?  6.0 (mat-at mat 0 0)))
  (test-assert (good-enough?  8.0 (mat-at mat 0 1))))

;;  gradient-descent
(let ([mat (gradient-descent
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((-3.0 4.0))) 1e-10 100)])
  (test-assert (good-enough? -2.999 (mat-at mat 0 0)))
  (test-assert (good-enough? 3.999 (mat-at mat 0 1))))

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
    (test-assert (good-enough? 1.054148091 (mat-at pred 0 0)))
    (test-assert (good-enough? 0.63071653 (mat-at pred 0 1)))
    (test-assert (good-enough? 1.1328074 (mat-at pred 0 2)))
    (test-equal #(2) (matrix-argmax pred))
    (test-assert (good-enough? 0.9280682857864075 (loss x t)))
    (test-equal #(2 3) (matrix-shape (numerical-gradient (lambda (w) (loss x t)) (get-w))))))


(let-values (([predict loss accuracy gradient update-params] (two-layer-net 3 10 3)))
  (test-equal #(2 3) (matrix-shape (predict (matrix-randn 2 3))))
  (loss (matrix-randn 2 3) (matrix ((0 0 1) (0 1 0))))
  (accuracy (matrix-randn 2 3) (matrix ((0 0 1) (0 1 0))))
  (gradient (matrix-randn 2 3)  (matrix ((0 0 1) (0 1 0)))))

(test-end)

;; Hyper parameters.
(define sanity-check? #t)
(define train-size (if sanity-check? 64 60000))
(define batch-size (if sanity-check? 8 100))
(define epochs (if sanity-check? 2 8))
(define hidden-size (if sanity-check? 4 50))
(define num-classes 10)
(define lr 0.1)
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
