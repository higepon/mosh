(define (nm:eval-str str)
  (ex:repl (call-with-port (open-string-input-port str) read)))

(define eval-r6rs 'INVALID-INIT5.SCM-VALUE-FATMOSH)
(define create-non-continuable-violation 'INVALID-INIT5.SCM-VALUE-FATMOSH)
