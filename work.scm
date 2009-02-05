(import (rnrs)
        (srfi :38)
        (rnrs mutable-pairs)
        )

(define p (cons 1 2))

(set-cdr! p p)

(write-with-shared-structure p)

;(display (microseconds))
;; (random-source-randomize! default-random-source)
;; (display (random-integer 10))
;; (newline)
;; (display (random-integer 10))
;; (newline)
;; (display (random-integer 10))
;; (newline)
;; (display (random-integer 10))
;; (newline)
