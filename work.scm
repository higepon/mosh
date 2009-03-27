(import (rnrs)
        (mosh)
        (srfi :64))

(define-record-type failure
  (fields
    (immutable expr)
    (immutable expected)
    (immutable actual)))

(define *nul* '*runner-nul*)

;; We may store #f as value of a-list.
;; So returns *nul* instead of #f.
(define (assq-ref obj alist)
  (let loop ([lst alist])
    (cond
     [(null? lst) *nul*]
     [(eq? (caar lst) obj)
      (cdar lst)]
     [else
      (loop (cdr lst))])))

(define (valid? obj)
  (not (eq? obj *nul*)))

(define-syntax with-color
  (lambda (x)
    (syntax-case x ()
      [(_ color expr more ...)
       (if (string=? (host-os) "win32")
           #'(begin expr more ...)
           #'(dynamic-wind
                 (lambda () (display color))
                 (lambda () expr more ...)
                 (lambda () (display "\x1b;[m")))]))))

(define-syntax with-color-green
  (lambda (x)
    (syntax-case x ()
      [(_ expr more ...)
       #'(with-color "\x1b;[0;32m" expr more ...)])))

(define-syntax with-color-red
  (lambda (x)
    (syntax-case x ()
      [(_ expr more ...)
       #'(with-color "\x1b;[0;31m" expr more ...)])))

(define (mosh-test-runner)
  (let ([runner (test-runner-null)]
        [failures '()])
    (define (add-failure! failure)
      (set! failures (cons failure failures)))
    (test-runner-on-test-end! runner
       (lambda (runner)
         (let* ([result (test-result-alist runner)]
                [kind (test-result-ref runner 'result-kind)])
           (when (memq kind '(fail))
             (add-failure! (make-failure
                            (assq-ref 'test-name result)
                            (assq-ref 'expected-value result)
                            (assq-ref 'actual-value result)))))))
    (test-runner-on-final! runner
       (lambda (runner)
         (cond
          [(> (test-runner-fail-count runner) 0)
           (with-color-red
            (format #t "[  FAILED  ] ~d passed, ~d failed.\n"
                    (test-runner-pass-count runner)
                    (test-runner-fail-count runner)))
           (for-each
            (lambda (f)
              (display "=======================================\n")
              (when (valid? (failure-expr f))
                (format (current-error-port) " Test     : ~a \n" (failure-expr f)))
              (when (valid? (failure-expected f))
                (format (current-error-port) " Expected : ~a \n" (failure-expected f)))
              (when (valid? (failure-actual f))
                (format (current-error-port) " Actual   : ~a \n" (failure-actual f))))
            failures)
           (display "=======================================\n")]
          [else
           (with-color-green
            (format #t "[  PASSED  ] ~d tests\x1b;[m\n" (test-runner-pass-count runner)))])))
    runner))

(test-runner-factory mosh-test-runner)

(define (test-not-match-name name)
  (lambda (runner)
    (not (equal? name (test-runner-test-name runner)))))


(test-begin "hige")
;(test-skip 2)

(test-begin "hage")
(test-skip (test-not-match-name "hage"))
;(test-skip 10)
(test-assert "hage" (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
(test-assert (let ([x 3]) x))
;(test-error lexical-violation? (vector-ref '#(1 2) 9))
;(test-eqv 4 (let ([x 3]) x))
(test-end)
(test-begin "hage")
;(test-eqv 5 6)
(test-end)
(test-end)
