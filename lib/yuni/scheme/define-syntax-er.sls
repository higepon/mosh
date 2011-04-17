(library (yuni scheme define-syntax-er)
         (export define-syntax/er)
         (import (rnrs)
                 (for (yuni scheme explicit-renaming) expand))
(define-syntax define-syntax/er
  (syntax-rules ()
    ((_ name translator)
     (define-syntax name (er-macro-transformer+ translator)))))
)
