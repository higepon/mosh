(import (rnrs)
        (rnrs r5rs)
        (rnrs eval))

(eval '(cond (#t => (lambda (x) x))) (null-environment 5))
