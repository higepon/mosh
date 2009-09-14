(import (rnrs)
        (mosh test)
        (prefix (mosh jit test) jit:))

(test-error assertion-violation? (vector-ref (vector) -1))
(test-error assertion-violation? (apply vector-ref (list (vector) -1)))
(test-error assertion-violation? (vector-set! (vector) -1 0))
(test-error assertion-violation? (apply vector-set! (list (vector) -1 0)))

(jit:test)

(test-results)
