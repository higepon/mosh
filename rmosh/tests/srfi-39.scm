(import (rnrs)
        (srfi :39)
        (mosh test))

(define a (make-parameter 1))
(define b (make-parameter 2 number->string))

(parameterize ((a 10)
               (b 20))
  (test-equal '(10 "20")
              (list (a) (b))))

(test-results)
