(import (rnrs)
        (mosh))

(define (prefix-inc prefix)
  (let ([len (string-length prefix)])
    (let loop ([i (- len 1)]
               [carry? #t]
               [accum '()])
      (cond
       [(< i 0)
        (if carry?
            (cons #\a accum)
            accum)]
       [carry?
        (let ([next-integer (+ 1 (char->integer (string-ref prefix i)))])
          (cond
           [(> next-integer (char->integer #\z))
            (loop (- i 1) #t (cons #\a accum))]
           [else
            (loop (- i 1) #f (cons (integer->char next-integer) accum))]))]
       [else
        (loop (- i 1) #f (cons (string-ref prefix i) accum))]))))

;; (let* ([prefix (call-with-input-file "./prefix.txt" read)]
;;        [next-prefix (prefix-inc prefix)])
;;   (format #t "prefix=~a next-prefix=~a\n" prefix next-prefix))


(display (prefix-inc "zz"))
