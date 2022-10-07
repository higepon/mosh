;; Some tests need (chibi test). This library aim to be able to replace it with (mosh chibi test).
(define-library (mosh chibi test)
  (export test-begin test-end test test-assert
          current-test-epsilon approx-equal? test-equal?
          test-true test-false test-eq test-eqv test-equal test-null
          test-write-equal
          test-error test-results fail
          test-error-string   ; exported for tests of xunit
          test-summary-string ; exported for tests of xunit
          good-enough?
          define-test test*
          test-skip
          test-values)
  (import (scheme base) (scheme complex) (except (mosh test) test-error))

  (define (test-begin . args) #f)

  (define (test-end . args) #f)

  (define-syntax test
    (syntax-rules ()
      ((_ expected expr)
       (if (number? expected)
           (test-true (number-equal? expected expr))
           (test-equal expected expr)))
      ((_ name expected expr)
       (test-equal expected expr))))

  (define-syntax test-error
    (syntax-rules ()
      ((_ expr)
       (test-true (guard (e (else #t))
                         (begin expr #f))))))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ str expr)
       (test-true expr))))

  (define current-test-epsilon (make-parameter 1e-5))

  (define (approx-equal? a b epsilon)
    (cond
     ((> (abs a) (abs b))
      (approx-equal? b a epsilon))
     ((zero? a)
      (< (abs b) epsilon))
     (else
      (< (abs (/ (- a b) b)) epsilon))))

  ;; From (chibi test)
  (define (test-equal? expect res)
    (or (equal? expect res)
        (if (real? expect)
            (and (inexact? expect)
                 (real? res)
                 ;; tests which expect an inexact value can
                 ;; accept an equivalent exact value
                 ;; (inexact? res)
                 (approx-equal? expect res (current-test-epsilon)))
            (and (complex? res)
                 (complex? expect)
                 (test-equal? (real-part expect) (real-part res))
                 (test-equal? (imag-part expect) (imag-part res))))))

  (define (number-equal? expect res)
    (or (equal? expect res)
        (if (real? expect)
            (and (inexact? expect)
                 (real? res)
                 ;; tests which expect an inexact value can
                 ;; accept an equivalent exact value
                 ;; (inexact? res)
                 (approx-equal? expect res (current-test-epsilon)))
            (and (complex? res)
                 (complex? expect)
                 (test-equal? (real-part expect) (real-part res))
                 (test-equal? (imag-part expect) (imag-part res))))))
)