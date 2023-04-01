;; FAKE inline library
(library (yuni util inline)
         (export define-inline)
         (import (rnrs))

;; 
(define-syntax define-inline
  (syntax-rules ()
    ((_ param ...)
     (define param ...))))

)
