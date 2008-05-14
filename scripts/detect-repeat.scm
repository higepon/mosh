#!/usr/bin/env gosh
(use srfi-1)
(use util.match)

;0.06
(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

(define (show-flonum num n)
  (receive (q r)
     (quotient&remainder (inexact->exact (round (* num (expt 10 n))))
                                         (expt 10 n))
    (format "~d.~v,'0d" q n r)))

(define (main args)
  (let1 ht (make-hash-table 'equal?)
    (let1 total
;        (with-input-from-port (current-input-port)
        (with-input-from-file (second args)
          (lambda ()
            (let loop ([obj (read)]
                       [prev 'prev]
                       [i   0])
              (cond
               [(eof-object? obj) i]
               [(symbol? obj)
                (aif (hash-table-get ht (cons prev obj) #f)
                     (hash-table-put! ht (cons prev obj) (+ 1 it))
                     (hash-table-put! ht (cons prev obj) 1))
                (loop (read) obj (+ 1 i))]
               [else
                (loop (read) prev (+ 1 i))]))))
      (print `(total ,total))
      (for-each print
                (take
                 (sort
                  (hash-table-map ht (lambda (k v)
                                       (list k v (show-flonum (* (/. v total) 100) 1))))
                  (lambda (a b) (> (second a) (second b))))
                 5)))))
