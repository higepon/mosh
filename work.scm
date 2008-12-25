(define (read-string s)
  (call-with-port
   (open-string-input-port s)
   read))
(write (read-string "#tあ"))
