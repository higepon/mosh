
(let1 pid (%fork)
  (if (zero? pid)
      (let1 p (open-file-output-port "/tmp/ls.txt")
        (%exec "ls" '("-la") #f p #f))
      (begin
        (%waitpid pid)
        (print 'done))))

