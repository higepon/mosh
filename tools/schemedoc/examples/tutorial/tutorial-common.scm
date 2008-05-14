; Common Scheme definitions for all LAML tutorial examples.

(define (laml-tutorial-url tutorial-name)
  (string-append 
     "../../../tutorial/" (as-string tutorial-name) "/" (as-string tutorial-name) "." "html"))