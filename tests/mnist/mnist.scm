(import (scheme base)
        (scheme process-context)
        (scheme write)
        (only (srfi 27) random-integer)
        (only (srfi 1) count second)
        (mnist loader)
        (mnist matrix)
        (mnist nn))

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
        (/ (inexact (count = (vector->list y) (vector->list t))) (matrix-shape x 0))))
    (define (update-params x t lr)
        (let-values ([[grad-w1 grad-b1 grad-w2 grad-b2] (gradient x t)])
          (set! w1 (matrix-sub w1 (matrix-multiply grad-w1 lr)))
          (set! b1 (matrix-sub b1 (matrix-multiply grad-b1 lr)))
          (set! w2 (matrix-sub w2 (matrix-multiply grad-w2 lr)))
          (set! b2 (matrix-sub b2 (matrix-multiply grad-b2 lr)))))
    (define (gradient x t)
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
    (values predict loss accuracy gradient update-params)))

(define (random-choice num-data batch-size)
  (map (lambda (_) (random-integer num-data)) (make-list batch-size)))

;; Hyper parameters.
(define sanity-check? #f)
(define train-size (if sanity-check? 64 60000))
(define test-size (if sanity-check? 8 10000))
(define batch-size (if sanity-check? 8 100))
(define epochs (if sanity-check? 2 8))
(define hidden-size (if sanity-check? 4 50))
(define num-classes 10)
(define lr 0.1)
(define iteration-per-epoch (/ train-size batch-size))

(let-values (([x-train t-train x-test t-test] (load-mnist (second (command-line)) train-size test-size)))
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
