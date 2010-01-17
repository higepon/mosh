(import (rnrs)
        (mosh test))

(define a 10)
(define a 20) ;; allows multiple definition
(test-equal 20 a)

(test-results)
