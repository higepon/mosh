#!/usr/bin/env ypsilon
;; Originally from Ypsilon Scheme
(import (core))

(add-load-path "./gambit-benchmarks")
(add-load-path "./bench/gambit-benchmarks")

(define-syntax time
  (syntax-rules ()
    ((_ expr)
     (destructuring-bind (real-start user-start sys-start) (time-usage)
       (let ((result (apply (lambda () expr) '())))
         (destructuring-bind (real-end user-end sys-end) (time-usage)
           (format #t
                   "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
                   (- real-end real-start)
                   (- user-end user-start)
                   (- sys-end sys-start)))
         result)))))

(define (run-benchmark name count ok? run-maker . args)
  (format #t "~%;;  ~a (x~a)~!" (pad-space name 7) count)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (and (not (ok? result)) (format #t "~%;; wrong result: ~s~%~!" result)))
  (format #t ";;  ----------------------------------------------------------------~!")
  (unspecified))

(define call-with-output-file/truncate
  (lambda (file-name proc)
    (let ((p (open-file-output-port
              file-name
              (file-options no-fail)
              (buffer-mode block)
              (native-transcoder))))
      (call-with-port p proc))))

(define fatal-error
  (lambda x
    (format #t "fatal-error: ~s" x)
    (exit)))

(define pad-space
  (lambda (str n)
    (let ((pad (- n (string-length str))))
      (if (<= pad 0)
          str
          (string-append str (make-string pad #\space))))))

(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
        (loop (+ i 1) (run))
        result)))

(define load-bench-n-run
  (lambda (name)
    (load (string-append name ".scm"))
    (main)))

(define-syntax time-bench
  (lambda (x)
    (syntax-case x ()
      ((?_ name count)
       (let ((symbolic-name (syntax->datum (syntax name))))
         (with-syntax ((symbol-iters (datum->syntax #'?_ (string->symbol (format "~a-iters" symbolic-name))))
                       (string-name (datum->syntax #'?_ (symbol->string symbolic-name))))
           (syntax
            (begin
              (define symbol-iters count)
              (load-bench-n-run string-name)))))))))

(define-syntax FLOATvector-const (syntax-rules () ((_ . lst) (list->vector 'lst))))
(define-syntax FLOATvector? (syntax-rules () ((_ x) (vector? x))))
(define-syntax FLOATvector (syntax-rules () ((_ . lst) (vector . lst))))
(define-syntax FLOATmake-vector (syntax-rules () ((_ n . init) (make-vector n . init))))
(define-syntax FLOATvector-ref (syntax-rules () ((_ v i) (vector-ref v i))))
(define-syntax FLOATvector-set! (syntax-rules () ((_ v i x) (vector-set! v i x))))
(define-syntax FLOATvector-length (syntax-rules () ((_ v) (vector-length v))))
(define-syntax nuc-const (syntax-rules () ((_ . lst) (list->vector 'lst))))
(define-syntax FLOAT+ (syntax-rules () ((_ . lst) (+ . lst))))
(define-syntax FLOAT- (syntax-rules () ((_ . lst) (- . lst))))
(define-syntax FLOAT* (syntax-rules () ((_ . lst) (* . lst))))
(define-syntax FLOAT/ (syntax-rules () ((_ . lst) (/ . lst))))
(define-syntax FLOAT= (syntax-rules () ((_ . lst) (= . lst))))
(define-syntax FLOAT< (syntax-rules () ((_ . lst) (< . lst))))
(define-syntax FLOAT<= (syntax-rules () ((_ . lst) (<= . lst))))
(define-syntax FLOAT> (syntax-rules () ((_ . lst) (> . lst))))
(define-syntax FLOAT>= (syntax-rules () ((_ . lst) (>= . lst))))
(define-syntax FLOATnegative? (syntax-rules () ((_ x) (negative? x))))
(define-syntax FLOATpositive? (syntax-rules () ((_ x) (positive? x))))
(define-syntax FLOATzero? (syntax-rules () ((_ x) (zero? x))))
(define-syntax FLOATabs (syntax-rules () ((_ x) (abs x))))
(define-syntax FLOATsin (syntax-rules () ((_ x) (sin x))))
(define-syntax FLOATcos (syntax-rules () ((_ x) (cos x))))
(define-syntax FLOATatan (syntax-rules () ((_ x) (atan x))))
(define-syntax FLOATsqrt (syntax-rules () ((_ x) (sqrt x))))
(define-syntax FLOATmin (syntax-rules () ((_ . lst) (min . lst))))
(define-syntax FLOATmax (syntax-rules () ((_ . lst) (max . lst))))
(define-syntax FLOATround (syntax-rules () ((_ x) (round x))))
(define-syntax FLOATinexact->exact (syntax-rules () ((_ x) (inexact->exact x))))
(define-syntax GENERIC+ (syntax-rules () ((_ . lst) (+ . lst))))
(define-syntax GENERIC- (syntax-rules () ((_ . lst) (- . lst))))
(define-syntax GENERIC* (syntax-rules () ((_ . lst) (* . lst))))
(define-syntax GENERIC/ (syntax-rules () ((_ . lst) (/ . lst))))
(define-syntax GENERICquotient (syntax-rules () ((_ x y) (quotient x y))))
(define-syntax GENERICremainder (syntax-rules () ((_ x y) (remainder x y))))
(define-syntax GENERICmodulo (syntax-rules () ((_ x y) (modulo x y))))
(define-syntax GENERIC= (syntax-rules () ((_ . lst) (= . lst))))
(define-syntax GENERIC< (syntax-rules () ((_ . lst) (< . lst))))
(define-syntax GENERIC<= (syntax-rules () ((_ . lst) (<= . lst))))
(define-syntax GENERIC> (syntax-rules () ((_ . lst) (> . lst))))
(define-syntax GENERIC>= (syntax-rules () ((_ . lst) (>= . lst))))
(define-syntax GENERICexpt (syntax-rules () ((_ x y) (expt x y))))

#!compatible

(format #t "\n\n;;  GABRIEL\n")
(time-bench boyer 3)
(time-bench browse 120)
(time-bench cpstak 80)
(time-bench ctak 25)
(time-bench dderiv 160000)
(time-bench deriv 320000)
(time-bench destruc 100)
(time-bench diviter 200000)
(time-bench divrec 140000)
(time-bench puzzle 12)
(time-bench takl 35)
(time-bench triangl 1)

(format #t "\n\n;;  ARITHMETIC\n")
(time-bench fft 200)
(time-bench fib 1)
(time-bench fibc 50)
(time-bench fibfp 1)
(time-bench mbrot 10)
;(time-bench nucleic 1)
;(time-bench pnpoly 10000)
(time-bench sum 1000)
(time-bench sumfp 600)
(time-bench tak 200)

(format #t "\n\n;;  MISCELLANEOUS\n")
(time-bench conform 4)
(time-bench earley 20)
(time-bench graphs 15)
(time-bench mazefun 100)
(time-bench nqueens 150)
(time-bench paraffins 100)
(time-bench peval 20)
(time-bench ray 1)
(time-bench scheme 3000)

(newline)
