(import (rnrs)
        )

(define (write-to-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))))

(write-to-file "./hige.txt" "abcde")
