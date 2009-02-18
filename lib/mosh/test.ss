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
      (test* (number? 'a) #f)
      (test-end))
    (end code)

    library: (mosh test)

    Unit Testing library
|#
(library (mosh test)
  (export test* test/exception test/violation? test/t test/f test-end)
  (import (only (rnrs) define define-syntax lambda let* if syntax-case syntax else +
                set! equal? quote begin syntax->datum exit ... guard violation? cons
                list cond > length display for-each current-error-port car cadr caddr _)
          (only (mosh) format)
          (only (system) make-parameter))

  (define error* (make-parameter '()))
  (define counter (make-parameter 0))
  (define ok-counter (make-parameter 0))

  #|
      Function: test*

      Run the test and compares its result with expected.

      Prototype:
      > (test* test expected)

      Parameters:

        test - test to run.
        expected - expected result.

      Returns:

        unspecified.
  |#
  (define-syntax test*
    (lambda (x)
      (syntax-case x ()
        ((_ form test expected)
         (syntax
          (let* ([result test]
                 [test-ok? (equal? result expected)])
            (cond
             [test-ok?
              (format #t "\rTest Running ~d/~d" (ok-counter) (counter))
              (counter (+ (counter) 1))
              (ok-counter (+ (ok-counter) 1))]
             [else
              (error* (cons (list form expected result) (error*)))
              (counter (+ (counter) 1))]
                ))))
        ((_ test expected)
         (syntax
          (test* 'test test expected ))))))

  #|
      Function: test/t

      Run the test and check the result is not #f.

      Prototype:
      > (test/t test)

      Parameters:

        test - test to run.

      Returns:

        unspecified.
  |#
  (define-syntax test/t
    (lambda (x)
      (syntax-case x ()
        [(_ form)
         #'(test* (if form #t #f) #t)])))

  #|
      Function: test/f

      Run the test and check the result is #f.

      Prototype:
      > (test/f test)

      Parameters:

        test - test to run.

      Returns:

        unspecified.
  |#
  (define-syntax test/f
    (lambda (x)
      (syntax-case x ()
        [(_ form)
         #'(test* form #f)])))


  #|
      Function: test/exception

      Run the test which may cause exception, and check the exception satisfies the pred?

      Prototype:
      > (test/exception pred? test ...)

      Parameters:

        pred? - pred?
        test - test to run.

      Returns:

        unspecified.
  |#
  (define-syntax test/exception
    (lambda (x)
      (syntax-case x ()
        ((_ pred? test ...)
         (syntax
          (test/t (guard (con [(pred? con) #t])
                         test ...)))))))

  #|
      Function: test/violation?

      Run the test which may cause exception, and check the exception satisfies the violation?

      Prototype:
      > (test/violation? test ...)

      Parameters:

        test - test to run.

      Returns:

        unspecified.
  |#
  (define-syntax test/violation?
    (lambda (x)
      (syntax-case x ()
        ((_ test ...)
         (syntax
          (test/exception violation? test ...))))))

  #|
      Function: test-end

      Show the test results.

      Prototype:
      > (test-end)

      Returns:

        unspecified.
  |#
  (define (test-end)
    (cond
     [(> (length (error*)) 0)
      (display "\n** Error(s)\n")
      (for-each
       (lambda (x)
         (format (current-error-port) "  ~a got ~a but ~a expected\n" (car x) (caddr x) (cadr x)))
       (error*))
      (exit -1)]
     [else
      (format #t "\rTest Running ~d/~d ... ok\n" (ok-counter) (counter))]))
)
