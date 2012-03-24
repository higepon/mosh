(library (nmosh applet genstub)
         (export (rename (genstub-cmd genstub)))
         (import (nmosh configure genstub)
                 (match)
                 (rnrs))

(define (genstub-cmd)
  (let ((c (command-line)))
    (match c
           ((_ in out) (genstub in out))
           (else
             (assert #f))))) 
)
