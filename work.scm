(let1 pid (%fork)
  (if (zero? pid)
      (%exec "hige" '("-la"))
      (begin
        (%waitpid pid)
        (print 'done))))
