(import (rnrs))
(define f (lambda () 123))
(write f)(newline)
(write (syntax->datum f))(newline)
