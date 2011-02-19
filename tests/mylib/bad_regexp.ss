#!r6rs
(library (bad_regexp)
   (export a)
   (import (rnrs))

;; regexp literal is invalid on #!r6rs mode.
(define a #/a/)
)
