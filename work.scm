;(import (rnrs)
;        (core)
;        (mosh))

#;(define-syntax time
  (syntax-rules ()
    ((_ expr)
     (destructuring-bind (real-start user-start sys-start) (time-usage)
       (let ((result (apply (lambda () expr) '())))
         (destructuring-bind (real-end user-end sys-end) (time-usage)
           (format #t
                   "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
                   (- real-end real-start)
                   (- user-end user-start)
                   (- sys-end sys-start)))
         result)))))

(define a '())
(let loop ([i 0])
  (cond
   [(= i 100000) '()]
   [else
    (%call/cc (lambda (c) (set! a 3)c))
    (loop (+ i 1))]))
