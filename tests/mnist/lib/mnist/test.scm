(import (scheme base))
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

(test-results)