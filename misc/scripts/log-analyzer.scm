#!/usr/bin/env gosh
(use srfi-1)
(use util.match)
(use file.util)

(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

;; input formt is following sexp
;; ('log name (t1-sec . t1-usec) (t2-sec . t2-usec))

(define (main args)
  (let1 times-hash (make-hash-table 'equal?)
    (for-each
     (lambda (result)
       (match result
         [('log name (t1-sec . t1-usec) (t2-sec . t2-usec))
          (let1 time (+ (* (- t2-sec t1-sec) 1000000) (- t2-usec t1-usec))
;            (print name time)
            (aif (hash-table-get times-hash name #f)
                 (hash-table-put! times-hash name (cons (+ (car it) 1) (+ (cdr it) time)))
                 (hash-table-put! times-hash name (cons 1 time))))]
         [else '()]))
     (file->sexp-list (second args)))
    (for-each
     print
     (sort (hash-table-map
            times-hash
            (lambda (key value)
              `(,key ,(car value) ,(/. (cdr value) 1000000))))
           (lambda (x y)
             (> (third x) (third y)))))))
