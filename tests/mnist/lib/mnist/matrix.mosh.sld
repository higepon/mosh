;; Header here.
(define-library (mnist matrix)
  (export bytevector->matrix matrix matrix-shape matrix-mul matrix-hstack-col matrix-add matrix-argmax mat-at matrix-divide matrix-map matrix-multiply matrix-randn matrix-slice matrix-stretch matrix-sub matrix-sum matrix-transpose matrix-vstack-row matrix-zeros-like matrix->list* vector-argmax one-hot)
  (export softmax) ;; TODO move to nn
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme inexact))
  (import (only (srfi 1) concatenate))
  (import (only (srfi 194) make-normal-generator))
  (import (rename (only (system) make-f64array f64array-ref f64array-set! f64array-shape f64array-dot-product)
                (f64array-dot-product matrix-mul) (f64array-shape matrix-shape))
)


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

(define (one-hot a num-class)
   (let ([mat (matrix (matrix-shape a 1) num-class 0.0)])
     (let loop ([i 0])
       (cond
         [(= i (matrix-shape a 1))
           mat]
         [else
           (mat-at mat i (exact (mat-at a 0 i)) 1.0)
           (loop (+ i 1))]))))

)