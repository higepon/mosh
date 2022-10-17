; matrix.sld - Matrix implementation for Mosh.
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
;; This version is using f64array family in Mosh.
(define-library (mnist matrix)
  (export matrix matrix-shape mat-at matrix-randn matrix-zeros-like)
  (export matrix-add matrix-mul matrix-divide matrix-sub matrix-sum matrix-multiply)
  (export matrix-hstack-col matrix-max matrix-argmax matrix-slice matrix-stretch matrix-transpose matrix-vstack-row vector-argmax matrix-map)
  (export bytevector->matrix matrix->list*)
  (export one-hot)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (only (srfi 1) concatenate))
  (import (only (srfi 194) make-normal-generator))
  (import (rename (only (system) make-f64array f64array-ref f64array-set! f64array-shape f64array-dot-product)
                (f64array-dot-product matrix-mul) (f64array-shape matrix-shape)))

;;; Create matrix.
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

;; Utlities.
(define (vector*->matrix v)
  (let* ([nrows (vec-len v)]
         [ncols (vec-len (vec-at v 0))]
         [mat (matrix nrows ncols)])
    (do ((i 0 (+ i 1)))
        ((= i nrows) mat)
      (do ((j 0 (+ j 1)))
          ((= j ncols))
        (mat-at mat i j (vec-at (vec-at v i) j))))))

(include "matrix-common.scm"))