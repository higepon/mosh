(import (rnrs)
        (match)
        (mosh internal library-converter)
        (mosh file)
        (only (mosh pp) pp)
        (only (srfi :1) first second)
        (mosh test))

(define (main args)
    (let ([exp (first (file->sexp-list (second args)))])
        (pp (rewrite-define-library "/path/to/library" exp))))

(main (command-line))
