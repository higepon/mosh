(import (rnrs)
        (mosh test))

;; R6RS doesn't say about follwing, but Mosh returns #t for-all.
(test-true (equal? +nan.0 +nan.0))
(test-true (equal? +nan.0 -nan.0))
(test-true (equal? -nan.0 +nan.0))
(test-true (equal? +nan.0 (string->number "+nan.0")))
(test-true (equal? +nan.0 (string->number "-nan.0")))
(test-true (equal? -nan.0 (string->number "+nan.0")))
(test-true (equal? -nan.0 (string->number "-nan.0")))
(test-results)
