; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Code for running and testing the reference implementation in Larceny.
; Larceny-specific code is marked by comments saying "[Larceny]".
; Portable code is marked by comments saying "[Portable]".

; [Larceny]  Enable SRFI-9 (Defining Record Types)
; and SRFI-23 (Error reporting mechanism).
;
; Note: SRFI-9 must be loaded before inlining of R5RS procedures
; is disabled.

(require 'srfi-9)
;(require 'srfi-23)

; [Larceny]  Disable inlining of R5RS procedures, etc.

(compiler-switches 'standard)
(include-source-code #t)


; [Portable]  Testing the sanity of various operations.

(define test-inputs0
  '(0 1 2 3 4 5 15 16 17 -1 -2 -3 -4 -5 -15 -16 -17))

(define test-inputs1
  (apply append
         (map (lambda (x) test-inputs0) test-inputs0)))

(define test-inputs2
  (apply append
         (map (lambda (x) (map (lambda (y) x) test-inputs0))
              test-inputs0)))

; [Portable]  Tests whether f and g behave the same on the above inputs.

(define (test-sanity f g)
  (for-each (lambda (x y)
              (if (not (equal? (f x y) (g x y)))
                  (begin (display "Sanity test failed on ")
                         (write x) (display " ") (write y)
                         (newline))))
            test-inputs1
            test-inputs2))
