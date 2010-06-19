; bytevector.scm - bytevector
;
;   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;  $Id: flonum.scm 621 2008-11-09 06:22:47Z higepon $

;; Originally from Ypsilon Scheme
(define-macro (unspecified)
  '(if #f #f))

(define-macro (div256 x)
  `(bitwise-arithmetic-shift ,x -8))

(define-macro (mod256 x)
  `(bitwise-and ,x 255))

(define (bytevector-uint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (let ((end (+ index size)))
           (let loop ((i index) (acc 0))
             (if (>= i end)
                 acc
                 (loop (+ i 1) (+ (* 256 acc) (bytevector-u8-ref bv i)))))))
        ((eq? endien 'little)
         (let loop ((i (+ index size -1)) (acc 0))
           (if (< i index)
               acc
               (loop (- i 1) (+ (* 256 acc) (bytevector-u8-ref bv i))))))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~a, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-sint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (if (> (bytevector-u8-ref bv index) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        ((eq? endien 'little)
         (if (> (bytevector-u8-ref bv (+ index size -1)) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~a, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-uint-set! bv index val endien size)
  (cond ((= val 0)
         (let ((end (+ index size)))
           (let loop ((i index))
             (cond ((>= i end) (unspecified))
                   (else
                    (bytevector-u8-set! bv i 0)
                    (loop (+ i 1)))))))
        ((< 0 val (expt 256 size))
         (cond ((eq? endien 'big)
                (let ((start (- (+ index size) 1)))
                  (let loop ((i start) (acc val))
                    (cond ((< i index) (unspecified))
                          (else
                           (bytevector-u8-set! bv i (mod256 acc))
                           (loop (- i 1) (div256 acc)))))))
               ((eq? endien 'little)
                (let ((end (+ index size)))
                  (let loop ((i index) (acc val))
                    (cond ((>= i end) (unspecified))
                          (else
                           (bytevector-u8-set! bv i (mod256 acc))
                           (loop (+ i 1) (div256 acc)))))))))
        (else
         (assertion-violation 'bytevector-uint-set!
                              (format "value out of range, ~s as argument 3" val)
                              (list bv index val endien size))))
  (unspecified))

(define (bytevector-sint-set! bv index val endien size)
  (let* ((p-bound (expt 2 (- (* size 8) 1))) 
         (n-bound (- (+ p-bound 1))))
    (if (< n-bound val p-bound)
        (if (> val 0)
            (bytevector-uint-set! bv index val endien size)
            (bytevector-uint-set! bv index (+ val (expt 256 size)) endien size))
        (assertion-violation 'bytevector-sint-set!
                             (format "value out of range, ~s as argument 3" val)
                             (list bv index val endien size))))
  (unspecified))

(define bytevector->uint-list
  (lambda (bv endien size)
    (let loop ((i (- (bytevector-length bv) size)) (acc '()))
      (if (> i -1)
          (loop (- i size) (cons (bytevector-uint-ref bv i endien size) acc))
          (if (= i (- size))
              acc
              (assertion-violation 'bytevector->uint-list
                                   (format "expected appropriate element size as argument 3, but got ~a" size)
                                   (list bv endien size)))))))

(define (bytevector->sint-list bv endien size)
  (let loop ((i (- (bytevector-length bv) size)) (acc '()))
    (if (> i -1)
        (loop (- i size) (cons (bytevector-sint-ref bv i endien size) acc))
        (if (= i (- size))
            acc
            (assertion-violation 'bytevector->sint-list
                                 (format "expected appropriate element size as argument 3, but got ~a" size)
                                 (list bv endien size))))))

(define (uint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-uint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))

(define (sint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-sint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))
