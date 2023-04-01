(library (yuni text styler)
         (export make-styler
                 styler-apply)
         (import (rnrs)
                 (srfi :26)
                 (shorten))
(define (take-style styler sym)
  (let ((r (assq sym styler)))
    (unless r
      (assertion-violation #f "error" sym styler))
    (cadr r)))

(define (styler-apply styler obj)
  (if (pair? obj)
    (let ((name (car obj))
          (args (cdr obj)))
      (apply (take-style styler name)
             (map (^e (if (pair? e) (styler-apply styler e) e))
                  args)))
    obj))

(define (make-styler env) env)

)
