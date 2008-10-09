(import (rnrs))

(define (list->vector2 l)
  (let* ((len (length l))
         (v (make-vector len)))
    (display len)
    (let loop ((l l) (pos 0))
      (if (not (null? l))
          (begin
            (vector-set! v pos (car l))
            (loop (cdr l) (+ pos 1)))))
    v))


(list->vector2 '(a . b))
