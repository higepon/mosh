;; (define (lcm2 a b)
;;   (/ (* a b) (gcd2 a b)))

;; (define (lcm . n*)
;;   (unless (for-all integer-valued? n*)
;;     (assertion-violation 'lcm "integer valued numbers required"))
;;   (case (length n*)
;;     [(0) 1]
;;     [(1) (abs (first n*))]
;;     [(2) (lcm2 (first n*) (second n*))]
;;     [else
;;      (apply lcm (lcm2 (first n*) (second n*)) (cddr n*))]))


;; (display (lcm 3 2 4.0))
;; (import (rnrs))
;; (display (log -1.000000-0.000000i))

(import (rnrs)
        (mosh string))

  (define-record-type err
    (fields err-c))

  (define-record-type expected-exception
    (fields))

  (define-record-type multiple-results
    (fields values))

  (define-record-type approx
    (fields value))

  (define-record-type alts
    (fields values))

  (define-syntax test
    (syntax-rules ()
      [(_ expr expected)
       (begin
         ;; (write 'expr) (newline)
         (run-test 'expr
                   (catch-exns (lambda () expr))
                   expected))]))

   (define (catch-exns thunk)
      (guard (c [#t (make-err c)])
        (call-with-values thunk
          (lambda x
            (if (= 1 (length x))
                (car x)
                (make-multiple-results x))))))


  (define (good-enough? x y)
    ;; relative error should be with 0.1%, but greater
    ;; relative error is allowed when the expected value
    ;; is near zero.
    (cond ((not (number? x)) #f)
          ((not (number? y)) #f)
          ((or (not (real? x))
               (not (real? y)))
           (and (good-enough? (real-part x) (real-part	y))
                (good-enough? (imag-part x) (imag-part	y))))
          ((infinite? x)
           (=	x (* 2.0 y)))
          ((infinite? y)
           (= (* 2.0 x) y))
          ((nan? y)
           (nan? x))
          ((> (magnitude y) 1e-6)
           (< (/ (magnitude (- x y))
                 (magnitude y))
              1e-3))
          (else
           (< (magnitude (- x y)) 1e-6))))

  (define (same-result? got expected)
    (cond
     [(and (real? expected) (nan? expected))
      (display "[1]")
      (and (real? got) (nan? got))]
     [(expected-exception? expected)
      (display "[2]")
      (expected-exception? got)]
     [(approx? expected)
      (display "[3]")
      (and (approx? got)
           (good-enough? (approx-value expected)
                         (approx-value got)))]
     [(multiple-results? expected)
      (and (multiple-results? got)
           (= (length (multiple-results-values expected))
              (length (multiple-results-values got)))
           (for-all same-result?
                    (multiple-results-values expected)
                    (multiple-results-values got)))]
     [(alts? expected)
      (exists (lambda (e) (same-result? got e))
              (alts-values expected))]
     [else 
      (display "[4]")
      (equal? got expected)]))


  (define checked 0)
  (define failures '())

  (define (run-test expr got expected)
    (display got)
    (display expected)
    (set! checked (+ 1 checked))
    (unless (same-result? got expected)
      (set! failures
            (cons (list expr got expected)
                  failures))))

  (define-syntax test-string-to-number
    (syntax-rules ()
      [(_ [str num] ...) (begin (test (string->number str) num) ...)]))


(test (string->number "0.1e100") (inexact (expt 10 99)))

(display failures)

