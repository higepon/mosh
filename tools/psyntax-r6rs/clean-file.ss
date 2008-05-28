#!/usr/bin/env petite --script

(print-brackets #f)
(print-vector-length #f)
(apply 
  (case-lambda
    [(script input output)
     (pretty-file input output)]
    [(script . args) (error #f "usage: ~a <input> <output>" script)])
  (command-line-arguments))

