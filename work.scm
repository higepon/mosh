(import (rnrs)
        (system)
        (mosh string))

;; (define (pipe2 cmd1 args1 cmd2 args2)
;;   (let-values ([(in out) (%pipe)])
;;     (%spawn cmd1 args1 (list #f out #f))
;;     (%spawn cmd2 args2 (list in #f #f))))

;; (define (pipe3 cmd1 args1 cmd2 args2 cmd3 args3)
;;   (let-values ([(in1 out1) (%pipe)])
;;       (%spawn cmd1 args1 (list #f out1 #f))
;;       (close-port out1)
;;     (let-values ([(in2 out2) (%pipe)])
;;       (%spawn cmd2 args2 (list in1 out2 #f))
;;       (close-port out2)
;;       (%spawn cmd3 args3 (list in2 #f #f))
;;       (close-port in1)
;;       (close-port in2)
;;       (%waitpid -1)
;;       (%waitpid -1)
;;       (%waitpid -1))))

;; (pipe3 "ls" '("-la") "grep" '("cpp") "grep" '("main"))

(define-syntax pipe
  (lambda (x)
    (syntax-case x ()
      [(_ (cmd1 args1) (cmd2 args2) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list #f out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) ...)
           (do ([i 0 (+ i 1)])
               ((= i (length '((cmd2 args2) ...))))
             (%waitpid -1)))]
      [(_ "internal" in (cmd args))
       #'(begin
           (%spawn cmd args (list in #f #f))
           (close-port in))]
      [(_ "internal" in (cmd1 args1) (cmd2 args2) (cmd3 args3) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list in out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) (cmd3 args3) ...))]
      )))

;(pipe ("ls" '()) ("grep" '("Pro")) ("grep" '("cpp")) ("grep" '("VM")))

(define-syntax ls
  (lambda (x)
    (syntax-case x ()
    [_ 3])))

(define-syntax ->
  (lambda (x)
    (syntax-case x ()
      [(_ (cmd1 args1 ...) (cmd2 args2 ...))
       #'(pipe ((symbol->string (syntax->datum #'cmd1)) (map symbol->string (syntax->datum #'(args1 ...))))
               ((symbol->string (syntax->datum #'cmd2)) (map symbol->string (syntax->datum #'(args2 ...)))))]
      [(_ cmd1 (cmd2 args2 ...))
       #'(pipe ((symbol->string (syntax->datum #'cmd1)) '())
               ((symbol->string (syntax->datum #'cmd2)) (map symbol->string (syntax->datum #'(args2 ...)))))]
      )))

(-> ls (grep Pro))
(newline)
;; (let-values ([(in1 out1) (%pipe)])
;;   (%spawn "ls" '() (list #f out1 #f))
;;   (close-port out1)
;;   (let-values ([(in2 out2) (%pipe)])
;;     (%spawn "grep" '("Pro") (list in1 out2 #f))
;;     (close-port out2)
;;     (let-values ([(in3 out3) (%pipe)])
;;       (%spawn "grep" '("cpp") (list in2 out3 #f))
;;       (close-port out3)
;;       (begin
;;              (%spawn "grep" '("VM") (list in3 #f #f))
;;              (close-port in3)))))
