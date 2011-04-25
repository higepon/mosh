(import (rnrs)
        (mosh)
        (only (mosh pp) pp)
        (mosh socket)
        (rnrs mutable-pairs)
        (mosh test))

(define-syntax test-print
  (lambda (x)
    (syntax-case x ()
      [(_ expected expr write-proc)
       (regexp? (syntax->datum #'expected))
       #'(test-true (expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc)))))]
      [(_ expected expr write-proc)
       #'(test-equal expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc))))])))

(define-syntax test-print*
  (lambda (x)
    (syntax-case x ()
      [(_ (expr expected) more ...)
       #'(test-print* (expr expected expected expected) more ...)]
      [(_ (expr display-expected write-expected) more ...)
       #'(test-print* (expr display-expected write-expected write-expected) more ...)]
      [(_ (expr display-expected write-expected pp-expected) more ...)
       #'(begin
           (test-print display-expected expr display)
           (test-print write-expected expr write)
           (test-print (if (string? pp-expected) (string-append pp-expected "\n") pp-expected) expr pp)
           (test-print* more ...))]
      [(_) #'#f])))

(define test-file (if (string=? (host-os) "mona") "/APPS/MOSH.APP/tests/test.txt" "tests/test.txt"))


(test-print* 
             [(make-client-socket "www.google.co.jp" "80") #/<socket client .*>/ #/<socket client .*>/ "#[socket]"]

)

(test-results)
