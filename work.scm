(import (system)
        (rnrs)
        (srfi :8))


(define (port->string p)
    (let loop ([ret '()][c (read-char p)])
      (if (eof-object? c)
          (list->string (reverse ret))
          (loop (cons c ret) (read-char p)))))

;; (receive (in out) (%pipe)
;;   (receive (pid cin cout cerr) (%spawn "ls" '("-a") (list #f out #f))
;;     (close-port out)
;;     (%waitpid pid)
;;     (display (port->string (transcoded-port in (make-transcoder (utf-8-codec)))))))
(define-syntax begin0
  (syntax-rules ()
    ((_ e es ...)
     (let ((v e))
       es ...
       v))))
(define (spawn->string command args)
    (let-values ([(in out) (%pipe)])
      (display "<1>")
      (let-values ([(pid cin cout cerr) (%spawn command args (list #f out #f))])
        (close-port out)
      (display "<2>")
      (display "<3>")

        (begin0
          (port->string (transcoded-port in (make-transcoder (utf-8-codec))))
          (close-port in)
        (%waitpid pid))

      )))
(display (spawn->string "ls" '("-la")))


;; (receive (in out) (%pipe)
;;   (let1 pid (%fork)
;;     (if (zero? pid)
;;         (%exec "ls" '("-la") #f out #f))
;;     (begin
;;       (%waitpid pid)
;;       (print (get-line (transcoded-port in (make-transcoder (utf-8-codec)))))
;;       (print 'done))))
