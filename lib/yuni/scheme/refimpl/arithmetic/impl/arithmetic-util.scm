(library (yuni scheme refimpl arithmetic impl arithmetic-util)
         (export make-typo-op/2
                 make-typo-op/1
                 never
                 always
                 id
                 one
                 one/flo)
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl flonum))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Utilities for implementing higher-level arithmetic

(define (make-typo-op/2 proc type)
  (lambda (a b)
    (error "type mismatch" proc type a b)))

(define (never x)
  #f)

(define (always x)
  #t)

(define (make-typo-op/1 proc type)
  (lambda (a)
    (error "type mismatch" proc type a)))

(define (id x) x)

(define (one x)
  (core->integer 1))

(define (one/flo x)
  (core->flonum 1.0))

)
