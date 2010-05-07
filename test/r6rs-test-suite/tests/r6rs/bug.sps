#!r6rs

(import (rnrs) (rnrs eval) (tests r6rs test))

(define-syntax test-library
   (syntax-rules ()
     [(_ test-proc library-name)
      (test/unspec (eval '(test-proc) (environment 'library-name)))]))

(test-library run-eval-tests (tests r6rs bug))

(report-test-results)

