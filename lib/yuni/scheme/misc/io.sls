(library (yuni scheme misc io)
         (export read write)
         (import (yuni scheme misc backend))

(define (do-read p)
  (get-datum p))

(define (do-write d p)
  (put-datum p d))

(define (read . x)
  (if (pair? x)
    (do-read (car x))
    (do-read (current-input-port))))

(define (write d . x)
  (if (pair? x)
    (do-write d (car x))
    (do-write d (current-output-port))))

)
                 
