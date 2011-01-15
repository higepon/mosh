(library (yuni scheme refimpl arithmetic impl nary)
         (export
           make-transitive-pred
           reduce
           make-min/max)
         (import (yuni scheme refimpl arithmetic backend))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Utilities for implementing n-ary procedures out of binary ones.

(define (make-transitive-pred pred)
  (lambda (arg1 arg2 . args)
    (cond
     ((pred arg1 arg2)
      (let loop ((last arg2) (args args))
	(cond
	 ((null? args) #t)
	 ((pred last (car args))
	  (loop (car args) (cdr args)))
	 (else #f))))
     (else #f))))

(define (reduce unit combine args)
  (cond
   ((null? args) unit)
   ((null? (cdr args)) (combine unit (car args)))
   (else
    (let loop ((acc (combine (car args) (cadr args))) (args (cddr args)))
      (if (null? args)
	  acc
	  (loop (combine acc (car args)) (cdr args)))))))

(define (make-min/max comp)
  (lambda (arg0 . args)
    (let loop ((m arg0) (args args))
      (cond 
       ((null? args) m)
       ((comp (car args) m) (loop (car args) (cdr args)))
       (else (loop m (cdr args)))))))
)
