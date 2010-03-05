(import (rnrs))

(define (func a b c) c)

(guard (c [#t #f])
       (func 1))

#;(guard (c [#t #f])
       (map 1))


(display "hello")


