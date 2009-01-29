(import (rnrs)
        (mosh process))

;; (if (fork)
;;     (display "hige")
;;     (display "hage"))

;; (let-values ([(pid cin cout cerr) (spawn "ls" '("-l") (list #f #f #f))])
;;   (waitpid pid))


(let-values ([(in out) (pipe)])
  (define (port->string p)
    (let loop ([ret '()][c (read-char p)])
      (if (eof-object? c)
          (list->string (reverse ret))
          (loop (cons c ret) (read-char p)))))
  (let-values ([(pid cin cout cerr) (spawn "ls" '("-l") (list #f out #f))])
    (close-port out)
    (write (port->string (transcoded-port in (make-transcoder (utf-8-codec)))))
    (close-port in)
    (waitpid pid)))
