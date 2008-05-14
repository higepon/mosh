#!/usr/bin/env gosh
(use srfi-1)
(use util.match)


;(write (proc-add-profile '(define (func x v w) (define b c) (define d e)  1 3)))


(define (proc-add-profile obj)
  (match obj
    [('define ('display . vars) . body)  ;; ignore lambda with internal define
     obj]
    [('define ('print . vars) . body) ;; ignore lambda with internal define
     obj]
    [('define (name . vars) ('define . _) . _) ;; ignore lambda with internal define
     (let loop ([body (cddr obj)]
                [ret '()])
       (match (car body)
         [('define x . y)
          (loop (cdr body) (append ret (list (car body))))]
         [else
          `(define (,name ,@vars)
             ,@ret
             (let ([before (get-timeofday)]
                   [ret (begin ,@body)]
                   [after (get-timeofday)])
               (dd "(")
               (dd (quote ,name))
               (dd " ")
               (dd before)
               (dd after)
          (if (= 0 (- (car after) (car before)))
              (begin
                (dd "(")
                (dd (- (cdr after) (cdr before)))
                (dd ")")))
               (dd ")\\n")
               ret))]))]
    [('define (name . vars) . body)
     `(define (,name ,@vars)
        (let ([before (get-timeofday)]
              [ret (begin ,@body)]
              [after (get-timeofday)])
          (dd "(")
          (dd (quote ,name))
          (dd " ")
          (dd before)
          (dd after)
          (if (= 0 (- (car after) (car before)))
              (begin
                (dd "(")
                (dd (- (cdr after) (cdr before)))
                (dd ")")))
          (dd ")\\n")
          ret))]
    [else obj]))

(define (main args)
  (with-input-from-file (second args)
    (lambda ()
      (let loop ([obj (read)])
        (cond
         [(eof-object? obj) '()]
         [else
          (write (proc-add-profile obj))
          (print "\n")
          (loop (read))]))))
  0)
