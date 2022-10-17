; matrix.sld - Matrix implementation written in R7RS + SRFI.
;
;   Copyright (c) 2022  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

;; We need some matrix operations support for MNIST implementation.
;; This version is written in R7RS and SRFI-1.
;; Matrix is represented as vector of vector such as #(#(3 4) #(1 2)).
(define-library (mnist matrix)
  (export matrix matrix-shape mat-at matrix-randn matrix-zeros-like)
  (export matrix-add matrix-mul matrix-divide matrix-sub matrix-sum matrix-multiply)
  (export matrix-hstack-col matrix-max matrix-argmax matrix-slice matrix-stretch matrix-transpose matrix-vstack-row vector-argmax matrix-map)
  (export bytevector->matrix matrix->list*)
  (export one-hot)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme inexact))
  (import (only (srfi 1) concatenate))
  (cond-expand
   [gauche
      (import (rename (data random) (reals-normal$ make-normal-generator)))]
   [(library (srfi 194))
     (import (only (srfi 194) make-normal-generator))])

(begin
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
       (vector-ref (vector-ref m i) j)]
      [(_ m i j value)
       (begin (vector-set! (vector-ref m i) j value) m)]))

  ;; Matrix multiplication.
  (define (matrix-mul a b)
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

  ;; Matrix shape.
  ;; N.B For now we only support 2D matrix.
  (define matrix-shape
    (case-lambda
     [(a) `#(,(vector-length a) ,(vector-length (vector-ref a 0)))]
     [(a n)
      (if (= n 0)
          (vector-length a)
          (vector-length (vector-ref a 0)))]))

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
        (vector-set! mat i (vector-copy (vector-ref row 0))))))

  ;; argmax
  (define (matrix-argmax a)
    (vector-map vector-argmax a))

  ;; Slice
  ;; TODO: Should this return copy?
  (define (matrix-slice a row-index*)
    (let ([mat (matrix (length row-index*) (matrix-shape a 1))])
      (for-each-with-index
       (lambda (i row-index)
         (vector-set! mat i (vector-ref a row-index)))
       row-index*)
      mat))


  )

(include "matrix-impl.scm")
)