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

    (mosh test) uses (srfi :64 testing) as backend.

    Example:
    (start code)
    (import (rnrs)
            (mosh test)
      (test-begin "number predicate")
      (test-false (number? 'a))
      (test-end))
    (end code)

    library: (mosh test)

    Unit Testing library
|#
(library (mosh test)
  (export    test-true test-false test-null test-external-rep test-no-error;; mosh only
   test-begin test-not-match-name
   test-end test-assert test-eqv test-eq test-equal
   test-approximate  test-error test-apply test-with-runner
   test-match-nth test-match-all test-match-any test-match-name
   test-skip test-expect-fail test-read-eval-string
   test-runner-group-path test-group-with-cleanup
   test-result-ref test-result-set! test-result-clear test-result-remove
   test-result-kind test-passed?
   test-log-to-file test-group)
  (import (only (rnrs) ... _ define define-record-type fields immutable let cond not eq? syntax-case string=? begin dynamic-wind lambda display define-syntax if syntax quote null? caar cdar else cdr let* when memq > for-each current-error-port set! cons member and boolean? let-values open-string-output-port write)
;   (rnrs)
          (rename (srfi :64 testing) (test-begin %test-begin))
          (only (mosh) format host-os))

  #|
      Function: test-begin

      A test-begin enters a new test group.
      The suite-name becomes the current test group name, and is added to the end of the test group path.

      Portable test suites should use a sting literal for suite-name; the effect of expressions or other kinds of literals is unspecified.

      Prototype:
      > (test-begin suite-name [count])

      Parameters:

        suite-name - test suite name.
        count - The optional count must match the number of test-cases executed by this group. (Nested test groups count as a single test case for this count.) This extra test may be useful to catch cases where a test doesn't get executed because of some unexpected error.

      Returns:

        unspecified.
  |#

  #|
      Function: test-end

      A test-end leaves the current test group. An error is reported if the suite-name does not match the current test group name.

      Additionally, if the matching test-begin installed a new test-runner, then the test-end will de-install it, after reporting the accumulated test results in an implementation-defined manner.

      Prototype:
      > (test-end [suite-name])

      Parameters:

        suite-name - test suite name.

      Returns:

        unspecified.
  |#

  #|
      Function: test-group

      This is usually equivalent to executing the decl-or-exprs within the named test group.

      However, the entire group is skipped if it matched an active test-skip (see later).

      Also, the test-end is executed in case of an exception.

      Equivalent to
      (start code)
      (if (not (test-to-skip% suite-name))
        (dynamic-wind
          (lambda () (test-begin suite-name))
          (lambda () decl-or-expr ...)
          (lambda () (test-end suite-name))))
      (end code)

      Prototype:
      > (test-group suite-name decl-or-expr ...)

      Parameters:

        suite-name - test suite name.
        decl-or-expr - decl-or-expr

      Returns:

        unspecified.
  |#


  #|
      Function: test-assert

      This evaluates the expression.

      The test passes if the result is true; if the result is false, a test failure is reported.
      The test also fails if an exception is raised.

      The test-name is a string that names the test case.

      (Though the test-name is a string literal in the examples, it is an expression.
      It is evaluated only once.)

      It is used when reporting errors, and also when skipping tests, as described below. It is an error to invoke test-assert if there is no current test runner.

      Prototype:
      > (test-assert [test-name] expression)

      Parameters:

        test-name - test name.
        expression - expression to evaluate.

      Returns:

        unspecified.
  |#

  #|
      Function: test-true

      Run the test and check the result is not #f.

      Prototype:
      > (test-true [test-name] expression)

      Parameters:

        test-name - test name.
        expression - expression to evaluate.

      Returns:

        unspecified.
  |#

#|
      Function: test-false

      Run the test and check the result is #f.

      Prototype:
      > (test-false [test-name] expression)

      Parameters:

        test-name - test name.
        expression - expression to evaluate.

      Returns:

        unspecified.
|#

#|
      Function: test-eqv

      This is equivalent to
      > (test-assert [test-name] (eqv? expected test-expr))

      Run the test and check the result is #f.

      Prototype:
      > (test-eqv [test-name] expected test-expr)

      Parameters:

        test-name - test name.
        expected - expected values
        test-expr - test-expr to evaluate.

      Returns:

        unspecified.
|#

#|
      Function: test-eq

      This is equivalent to
      > (test-assert [test-name] (eq? expected test-expr))

      Run the test and check the result is #f.

      Prototype:
      > (test-eq [test-name] expected test-expr)

      Parameters:

        test-name - test name.
        expected - expected values
        test-expr - test-expr to evaluate.

      Returns:

        unspecified.
|#

#|
      Function: test-equal

      This is equivalent to
      > (test-assert [test-name] (equal? expected test-expr))

      Run the test and check the result is #f.

      Prototype:
      > (test-equal [test-name] expected test-expr)

      Parameters:

        test-name - test name.
        expected - expected values
        test-expr - test-expr to evaluate.

      Returns:

        unspecified.
|#

#|
      Function: test-approximate

      This is equivalent to (except that each argument is only evaluated once)
      (start code)
      (test-assert [test-name]
        (and (>= test-expr (- expected error))
         (<= test-expr (+ expected error))))
      (end code)

      Run the test and check the result is #f.

      Prototype:
      > (test-approximate [test-name] expected test-expr error)

      Parameters:

        test-name - test name.
        expected - expected values
        test-expr - test-expr to evaluate.
        error - allowed error.

      Returns:

        unspecified.
|#

#|
      Function: test-read-eval-string

      This function parses string (using read) and evaluates the result.

      The result of evaluation is returned from test-read-eval-string. An error is signalled if there are unread characters after the read is done.

      Prototype:
      > (test-read-eval-string string)

      Parameters:

        string - string to evaluate.

      Returns:

        evalated result.
|#

#|
      Function: test-error

      Evaluating test-expr is expected to signal an error.The kind of error is indicated by error-type.

      If the error-type is left out, or it is #t, it means "some kind of unspecified error should be signaled".


      Prototype:
      > (test-error [[test-name] error-type] test-expr)

      Parameters:

        test-name - test name.
        error-type - error-type
        test-expr - test-expr to evaluate.

      Returns:

        unspecified.
|#


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
            (format #t "[  PASSED  ] ~d tests\n" (test-runner-pass-count runner)))])))
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

(define (%test-external-rep tname expected obj)
  (let-values ([(port get-string) (open-string-output-port)])
    (write obj port)
    (test-equal tname expected (get-string))))

(define-syntax test-external-rep
  (lambda (x)
    (syntax-case x ()
      [(_ tname expected expr)
       #'(%test-external-rep tname expected expr)]
      [(_ expected expr)
       #'(%test-external-rep 'expr expected expr)])))

(define-syntax test-no-error
  (lambda (x)
    (syntax-case x ()
      [(_ tname expr)
       #'(test-assert tname expr)]
      [(_ expr)
       #'(test-assert 'expr expr)])))

)
