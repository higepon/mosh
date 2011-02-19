(library (good_regexp)
   (export a)
   (import (rnrs))

;; regexp literal is valid on not #!r6rs mode.
(define a #/a/)
)
