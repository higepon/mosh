
;;;; quux.ss
(library (foo)
 (export f)
 (import (rnrs))

 (define f 0))

(library (bar)
 (export g)
 (import (rnrs))

 (define g 1))

(import (rnrs) (foo) (bar))
;; top-level program goes here

(display g)
