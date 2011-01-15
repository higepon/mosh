(library (yuni util combinators)
         (import (rnrs))
         (export compose)

;; gauche .$, compose
(define (compose . args)
  ;; from Gauche manual:
  ;; (compose f g) ==
  ;;   (lambda args 
  ;;     (call-with-values 
  ;;       (lambda () (apply g args)) 
  ;;       f))
  ;;
  ;; expanded.
  (if (pair? args)
    (let ((f (car args))
          (next (cdr args)))
      (if (pair? next)
        (let ((g (car next))
              (rest (cdr next)))
          (let ((q (lambda X 
                     (call-with-values (lambda () (apply g X)) f))))
            (if (null? rest)
              q
              (apply compose (cons q rest)))))
        f))
    values))

)
