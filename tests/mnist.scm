(import (scheme base)
        (scheme write)
        (only (mosh) format)
        (mosh test))

;; List of list to vector of vector.
(define (list*->vector* l*)
    (list->vector (map (lambda (l) (list->vector l)) l*)))

;; Create a nested vector.
(define (make-vector* m n)
    (let ([v (make-vector m)])
      (do ((i 0 (+ i 1)))
          ((= i m) v)
          (vec-at v i (make-vector n)))))

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
    (make-vector* m n)]))

;; Get or set (i, j) element of matrix.
(define-syntax mat-at
    (syntax-rules ()
      [(_ m i j)
        (vec-at (vec-at m i) j)]
      [(_ m i j value)
        (vec-at (vec-at m i) j value)]))

;; Matrix shape.
;; N.B For now we only support 2D matrix.
(define (matrix-shape x)
    `#(,(vec-len x) ,(vec-len (vec-at x 0))))

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



;; Matrix shape.
(test-equal '#(1 2) (matrix-shape (matrix ((1 2)))))
(test-equal '#(2 2) (matrix-shape (matrix ((1 2) (3 4)))))

;; Matrix accessor get.
(test-equal 2 (mat-at (matrix ((1 2) (3 4))) 0 1))

;; Matrix accessor set.
(let ([m (matrix ((1 2) (3 4)))])
    (mat-at m 1 0 5)
    (test-equal #(#(1 2) #(5 4)) m))

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

(test-results)
