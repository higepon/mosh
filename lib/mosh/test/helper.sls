(library (mosh test helper)
         (export stage-test
                 run-staged-tests)
         (import (rnrs) (mosh))

(define test* (make-eq-hashtable))

(define (stage-test name proc)
  (hashtable-set! test* name proc))

(define (run-staged-tests)
  (hashtable-for-each (lambda (key proc)
                        (proc))
                      test*))

)
