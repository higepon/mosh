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

(define (one-hot a num-class)
   (let ([mat (matrix (matrix-shape a 1) num-class 0.0)])
     (let loop ([i 0])
       (cond
         [(= i (matrix-shape a 1))
           mat]
         [else
           (mat-at mat i (exact (mat-at a 0 i)) 1.0)
           (loop (+ i 1))]))))