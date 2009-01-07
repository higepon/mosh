
(let1 pid (%fork)
  (if (zero? pid)
      (%exec "ls" '("-la"))
      (begin
        (%waitpid pid)
        (print 'done))))

