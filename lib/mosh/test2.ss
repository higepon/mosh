(library (mosh test2)
  (export    test-true test-false ;; mosh only
   test-begin test-not-match-name 
   test-end test-assert test-eqv test-eq test-equal
   test-approximate  test-error test-apply test-with-runner
   test-match-nth test-match-all test-match-any test-match-name
   test-skip test-expect-fail test-read-eval-string
   test-runner-group-path test-group-with-cleanup
   test-result-ref test-result-set! test-result-clear test-result-remove
   test-result-kind test-passed?
   test-log-to-file test-group)
  (import (only (rnrs) ... _ define define-record-type fields immutable let cond not eq? syntax-case string=? begin dynamic-wind lambda display define-syntax if syntax quote null? caar cdar else cdr let* when memq > for-each current-error-port set! cons member and boolean?)
          (rename (srfi :64 testing) (test-begin %test-begin))
          (only (mosh) format host-os))

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

(define (test-not-match-name name)
  (lambda (runner)
    (not (member name (test-runner-group-stack runner)))))

(define-syntax test-begin
  (lambda (x)
    (syntax-case x ()
      [(_ args ...)
      #'(begin (test-runner-factory mosh-test-runner)
               (%test-begin args ...))])))

(define-syntax test-true
  (lambda (x)
    (syntax-case x ()
      [(_ tname expr)
       #'(test-assert tname (let ([x expr]) (and (boolean? x) x)))]
      [(_ expr)
       #'(test-assert (let ([x expr]) (and (boolean? x) x)))])))

(define-syntax test-false
  (lambda (x)
    (syntax-case x ()
      [(_ tname expr)
       #'(test-assert tname (let ([x expr]) (and (boolean? x) (not x))))]
      [(_ expr)
       #'(test-assert (let ([x expr]) (and (boolean? x) (not x))))])))


)
