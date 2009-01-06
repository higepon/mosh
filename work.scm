(import (rnrs)
        (mosh pp))

(pretty-print '(let ([a 3]) a))

;; (let1 pid (%fork)
;;   (if (zero? pid)
;;       (%exec "hige" '("-la"))
;;       (begin
;;         (%waitpid pid)
;;         (print 'done))))
