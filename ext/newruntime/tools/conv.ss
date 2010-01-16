(import (rnrs))

(define (itr)
  (let ((l (read)))
    (cond
      ((eof-object? l) (exit))
      (else (write l) (itr)))))

(itr)
