(import (rnrs)
        (mosh))

;; increment alphabet symbol
;; (ex) a   -> b
;;      z   -> A
;;      abZ -> aca
(define (prefix-inc prefix-string)
  (let* ([prefix (symbol->string prefix-string)]
         [len (string-length prefix)])
    (let loop ([i (- len 1)]
               [carry? #t]
               [accum '()])
      (cond
       [(< i 0)
        (string->symbol
         (list->string (if carry?
                           (cons #\a accum)
                           accum)))]
       [carry?
        (let ([next-integer (+ 1 (char->integer (string-ref prefix i)))])
          (cond
           [(= next-integer 123) ;; (+ (char->integer #\z) 1) => 123
            (loop (- i 1) #f (cons #\A accum))]
           [(= next-integer 91) ;; (+ (char->integer #\Z) 1) => 90
            (loop (- i 1) #t (cons #\a accum))]
           [else
            (loop (- i 1) #f (cons (integer->char next-integer) accum))]))]
       [else
        (loop (- i 1) #f (cons (string-ref prefix i) accum))]))))

(define (prefix-inc! file)
  (let* ([prefix (call-with-input-file file read)]
         [next-prefix (prefix-inc prefix)])
    (call-with-output-file file (lambda (port) (write next-prefix port)))
    prefix))

(write (prefix-inc! "./prefix.txt"))

