(library (yuni scheme refimpl arithmetic impl custom)
         (export
           *width*
           *fixnums-are-records*
           *flonums-are-records*
           define-record-discloser)
         (import
           (yuni scheme refimpl arithmetic backend))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Code for customizing the reference implementation.
;
; Before loading this file, any inlining of R5RS arithmetic
; procedures must be disabled, and SRFI 9 (Defining Record
; Types) and SRFI 23 (Error reporting mechanism) must be
; enabled.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The fixnum range is a half-open two's complement range.
;
; To use a smaller or larger range of fixnums, just change
; the following constant to some other positive even integer.
; It can't be too small, though, because the precision of bignums
; is likely to be limited to (* 1/4 *width* (expt 2 *width*)) bits.
;
; FIXME:
; The width must be even.
; (This simplifies multiplication of fixnums.)
;
; FIXME:
; The current implementation appears to work for 12-bit fixnums,
; but not for 8-bit fixnums.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *width* 24)

; Define as #t if fixnums are represented as records.
; Otherwise fixnums will be represented as exact integers.

(define *fixnums-are-records* #f)

; Define as #t if flonums are represented as records.
; Otherwise flonums will be represented as inexact reals.

(define *flonums-are-records* #t)






; Eliminate Scheme48 dependencies.
; If you're using Scheme 48, just comment this out.
;'

(define (define-record-discloser . args) #f)

)
