(library (nmosh applet swank)
         (export swank)
         (import (rnrs) (swank))

(define (swank)
  (start-server #f)))
