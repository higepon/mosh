(receive (in out) (%pipe)
  (let1 pid (%fork)
    (if (zero? pid)
        (%exec "ls" '("-la") #f out #f))
    (begin
      (%waitpid pid)
      (print (get-line (transcoded-port in (make-transcoder (utf-8-codec)))))
      (print 'done))))

