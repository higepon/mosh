(import (rnrs)
        (mosh)
        (srfi :1))

(define (pre-compile file)
  ((symbol-value 'pre-compile-r6rs-file) file))

(define (main args)
  (cond
   [(= 1 (length args))
    (display "top-level-program file is required\n" (current-error-port))]
   [else
    (pre-compile (second args))]))

(main (command-line))
