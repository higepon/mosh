; test.ss - Test
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;  $Id: test.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Unit Testing

    Defines a set of functions to write test scripts.

    Example:
    (start code)
    (import (rnrs)
            (mosh test)
      (test-false (number? 'a))
      (test-end))
    (end code)

    library: (mosh test)

    Unit Testing library
|#
;; (library (mosh test)
;;   (export test* test/exception test/violation? test/t test/f test-end)
;;   (import (only (rnrs) define define-syntax lambda let* if syntax-case syntax else + raise make-error
;;                 set! equal? quote begin syntax->datum exit ... guard violation? cons
;;                 list cond > length display for-each current-error-port car cadr caddr _)
;;           (only (mosh) format)
;;           (only (system) make-parameter))

;;   (define error* (make-parameter '()))
;;   (define counter (make-parameter 0))
;;   (define ok-counter (make-parameter 0))

;;   #|
;;       Function: test*

;;       Run the test and compares its result with expected.

;;       Prototype:
;;       > (test* test expected)

;;       Parameters:

;;         test - test to run.
;;         expected - expected result.

;;       Returns:

;;         unspecified.
;;   |#
;;   (define-syntax test*
;;     (lambda (x)
;;       (syntax-case x ()
;;         ((_ form test expected)
;;          (syntax
;;           (let* ([result test]
;;                  [test-ok? (equal? result expected)])
;;             (cond
;;              [test-ok?
;;               (format #t "\rTest Running ~d/~d" (ok-counter) (counter))
;;               (counter (+ (counter) 1))
;;               (ok-counter (+ (ok-counter) 1))]
;;              [else
;;               (error* (cons (list form expected result) (error*)))
;;               (counter (+ (counter) 1))]
;;                 ))))
;;         ((_ test expected)
;;          (syntax
;;           (test* 'test test expected ))))))

;;   #|
;;       Function: test/t

;;       Run the test and check the result is not #f.

;;       Prototype:
;;       > (test/t test)

;;       Parameters:

;;         test - test to run.

;;       Returns:

;;         unspecified.
;;   |#
;;   (define-syntax test/t
;;     (lambda (x)
;;       (syntax-case x ()
;;         [(_ form)
;;          #'(test* (if form #t #f) #t)])))

;;   #|
;;       Function: test/f

;;       Run the test and check the result is #f.

;;       Prototype:
;;       > (test/f test)

;;       Parameters:

;;         test - test to run.

;;       Returns:

;;         unspecified.
;;   |#
;;   (define-syntax test/f
;;     (lambda (x)
;;       (syntax-case x ()
;;         [(_ form)
;;          #'(test* form #f)])))


;;   #|
;;       Function: test/exception

;;       Run the test which may cause exception, and check the exception satisfies the pred?

;;       Prototype:
;;       > (test/exception pred? test ...)

;;       Parameters:

;;         pred? - pred?
;;         test - test to run.

;;       Returns:

;;         unspecified.
;;   |#
;;   (define-syntax test/exception
;;     (lambda (x)
;;       (syntax-case x ()
;;         ((_ pred? test ...)
;;          (syntax
;;           (test/t (guard (con [(pred? con) (display "here")#t][else #f])
;;                          test ... (raise (make-error)))))))))

;;   #|
;;       Function: test/violation?

;;       Run the test which may cause exception, and check the exception satisfies the violation?

;;       Prototype:
;;       > (test/violation? test ...)

;;       Parameters:

;;         test - test to run.

;;       Returns:

;;         unspecified.
;;   |#
;;   (define-syntax test/violation?
;;     (lambda (x)
;;       (syntax-case x ()
;;         ((_ test ...)
;;          (syntax
;;           (test/exception violation? test ...))))))

;;   #|
;;       Function: test-end

;;       Show the test results.

;;       Prototype:
;;       > (test-end)

;;       Returns:

;;         unspecified.
;;   |#
;;   (define (test-end)
;;     (cond
;;      [(> (length (error*)) 0)
;;       (display "\n** Error(s)\n")
;;       (for-each
;;        (lambda (x)
;;          (format (current-error-port) "  ~a got ~a but ~a expected\n" (car x) (caddr x) (cadr x)))
;;        (error*))
;;       (exit -1)]
;;      [else
;;       (format #t "\rTest Running ~d/~d ... ok\n" (ok-counter) (counter))]))
;; )


(library (mosh test2)
  (export    test-true test-false test-null;; mosh only
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
           (display "=======================================\n")
           (with-color-red
            (format #t "[  FAILED  ] ~d passed, ~d failed.\n"
                    (test-runner-pass-count runner)
                    (test-runner-fail-count runner)))]
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

(define-syntax test-null
  (lambda (x)
    (syntax-case x ()
      [(_ tname expr)
       #'(test-assert tname (null? expr))]
      [(_ expr)
       #'(test-assert (null? expr))])))


)
