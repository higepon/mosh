(import (scheme base))
(import (scheme case-lambda))
(import (mnist matrix) (mnist nn))
(import (mosh test))

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

;; Simple Net
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

;; numerical-gradient
(let ([mat (numerical-gradient
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((3.0 4.0))))])
  (test-true (good-enough?  6.0 (mat-at mat 0 0)))
  (test-true (good-enough?  8.0 (mat-at mat 0 1))))

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

;;  gradient-descent
(let ([mat (gradient-descent
            (lambda (x)
              (let ([x0 (mat-at x 0 0)]
                    [x1 (mat-at x 0 1)])
                (+ (* x0 x0) (* x1 x1))))
            (matrix ((-3.0 4.0))) 1e-10 100)])
  (test-true (good-enough? -2.999 (mat-at mat 0 0)))
  (test-true (good-enough? 3.999 (mat-at mat 0 1))))

(test-results)