;; (import (rnrs))
;; (define list-length
;;     (lambda (obj)
;;       (call-with-current-continuation
;;        (lambda (return)
;;          (letrec ((r
;;                    (lambda (obj)
;;                      (cond ((null? obj) 0)
;;                            ((pair? obj)
;;                             (+ (r (cdr obj)) 1))
;;                            (else (return #f))))))
;;            (r obj))))))

;; (display (list-length '(1 2 3 3)))

(define Gb@G1@list-length '())
((lambda (G1@list-length G2@dummy) (begin (set! G1@list-length (lambda (G3@obj) (call-with-current-continuation (lambda (G5@return) (letrec ((G7@r (lambda (G9@obj) (if (null? G9@obj) '0 (if (pair? G9@obj) (+ (G7@r (cdr G9@obj)) '1) (G5@return '#f)))))) (G7@r G3@obj)))))) (set! G2@dummy (begin (display (G1@list-length '(1 2 3 3))))) (set! Gb@G1@list-length G1@list-length))) '#f '#f)
