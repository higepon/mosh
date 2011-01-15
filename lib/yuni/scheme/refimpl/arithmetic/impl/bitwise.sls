;; bitwise-arithmetic took from r5rs.sch

(library (yuni scheme refimpl arithmetic impl bitwise)
         (export arithmetic-shift
                 bitwise-and
                 bitwise-ior
                 bitwise-xor
                 bitwise-not
                 bit-count)
         (import (yuni scheme refimpl arithmetic backend))


; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Code for loading and testing the reference implementation into
; R5RS-conforming implementations of Scheme that provide flonum
; arithmetic and exact integer arithmetic over [-2^23, 2^23-1].
;
; Before loading this file, any inlining of R5RS arithmetic
; procedures must be disabled, and SRFI 9 (Defining Record
; Types) and SRFI 23 (Error reporting mechanism) must be
; enabled.

; Define a small set of bitwise operators.
; The core: procedures will be loaded soon afterwards.

; Given exact integers n and k, with k >= 0, return (* n (expt 2 k)).

(define (arithmetic-shift n k)
  (if (and (core:exact? n)
           (core:integer? n)
           (core:exact? k)
           (core:integer? k))
      (cond ((core:> k 0)
             (core:* n (core:expt 2 k)))
            ((core:= k 0)
             n)
            ((core:>= n 0)
             (core:quotient n (core:expt 2 (core:- k))))
            (else
             (let* ((q (core:expt 2 (core:- k)))
                    (p (core:quotient (core:- n) q)))
               (if (core:= n (core:* p k))
                   (core:- p)
                   (core:- -1 p)))))
      (error "illegal argument to arithmetic-shift" n k)))

; Bitwise operations on exact integers.

(define (bitwise-and i j)
  (if (and (core:exact? i)
           (core:integer? i)
           (core:exact? j)
           (core:integer? j))
      (cond ((or (core:= i 0) (core:= j 0))
             0)
            ((core:= i -1)
             j)
            ((core:= j -1)
             i)
            (else
             (let* ((i0 (if (core:odd? i) 1 0))
                    (j0 (if (core:odd? j) 1 0))
                    (i1 (core:- i i0))
                    (j1 (core:- j j0))
                    (i/2 (core:quotient i1 2))
                    (j/2 (core:quotient j1 2))
                    (hi (core:* 2 (bitwise-and i/2 j/2)))
                    (lo (core:* i0 j0)))
               (core:+ hi lo))))
      (error "illegal argument to bitwise-and" i j)))

(define (bitwise-ior i j)
  (if (and (core:exact? i)
           (core:integer? i)
           (core:exact? j)
           (core:integer? j))
      (cond ((or (core:= i -1) (core:= j -1))
             -1)
            ((core:= i 0)
             j)
            ((core:= j 0)
             i)
            (else
             (let* ((i0 (if (core:odd? i) 1 0))
                    (j0 (if (core:odd? j) 1 0))
                    (i1 (core:- i i0))
                    (j1 (core:- j j0))
                    (i/2 (core:quotient i1 2))
                    (j/2 (core:quotient j1 2))
                    (hi (core:* 2 (bitwise-ior i/2 j/2)))
                    (lo (if (core:= 0 (core:+ i0 j0)) 0 1)))
               (core:+ hi lo))))
      (error "illegal argument to bitwise-ior" i j)))

(define (bitwise-xor i j)
  (if (and (core:exact? i)
           (core:integer? i)
           (core:exact? j)
           (core:integer? j))
      (cond ((and (core:= i -1) (core:= j -1))
             0)
            ((core:= i 0)
             j)
            ((core:= j 0)
             i)
            (else
             (let* ((i0 (if (core:odd? i) 1 0))
                    (j0 (if (core:odd? j) 1 0))
                    (i1 (core:- i i0))
                    (j1 (core:- j j0))
                    (i/2 (core:quotient i1 2))
                    (j/2 (core:quotient j1 2))
                    (hi (core:* 2 (bitwise-xor i/2 j/2)))
                    (lo (if (core:= 1 (core:+ i0 j0)) 1 0)))
               (core:+ hi lo))))
      (error "illegal argument to bitwise-xor" i j)))

(define (bitwise-not i)
  (if (and (core:exact? i)
           (core:integer? i))
      (cond ((core:= i -1)
             0)
            ((core:= i 0)
             -1)
            (else
             (let* ((i0 (if (core:odd? i) 1 0))
                    (i1 (core:- i i0))
                    (i/2 (core:quotient i1 2))
                    (hi (core:* 2 (bitwise-not i/2)))
                    (lo (core:- 1 i0)))
               (core:+ hi lo))))
      (error "illegal argument to bitwise-not" i)))

(define (bit-count i)
  (if (and (core:exact? i)
           (core:integer? i))
      (cond ((core:= i -1)
             0)
            ((core:= i 0)
             0)
            (else
             (let* ((i0 (if (core:odd? i) 1 0))
                    (i1 (core:- i i0))
                    (i/2 (core:quotient i1 2))
                    (hi (bit-count i/2))
                    (lo (if (core:> i 0) i0 (core:- 1 i0))))
               (core:+ hi lo))))
      (error "illegal argument to bit-count" i)))

)
