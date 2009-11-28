#!/usr/bin/env gosh
(use srfi-1)
(use util.match)

(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

(define (main args)
  (define (analyze obj)
    (match obj
      [(name bsec busec asec ausec)
       (if (zero? (- asec bsec))
           `(,name ,(- ausec busec))
           '())]
      [else '()]))
  (let1 table (make-hash-table)
    (with-input-from-port (current-input-port)
      (lambda ()
        (let loop ([obj (read)]
                   [i   0])
          (cond
           [(eof-object? obj)
            (for-each (lambda (x) (write x) (print "")) (sort(hash-table-map table (lambda (key val) `(,key ,(car val) ,(cdr val) ,(/. (car val) (cdr val))))) (lambda (x y) (> (second x) (second y)))))]
           [else
            (if (zero? (modulo i 10))
                (begin (display i (current-error-port)) (display #\newline (current-error-port))))
            (let1 o (analyze obj)
              (if (pair? o)
                  (aif (hash-table-get table (first o) #f)
                       (hash-table-put! table (first o) (cons (+ (second o) (car it)) (+ 1 (cdr it))))
                       (hash-table-put! table (first o) (cons (second o) 1))))
              (loop (read) (+ i 1)))])))))
    0)
