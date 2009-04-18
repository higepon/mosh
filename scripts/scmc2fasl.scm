(import (rnrs)
        (mosh))

(define (extract obj)
  (cond
   [(vector? obj)
    (vector-map extract obj)]
   [(and (pair? obj) (eq? (car obj) '*insn*))
    (make-instruction (cadr obj))]
   [(and (pair? obj) (eq? (car obj) '*compiler-insn*))
    (make-compiler-instruction (cadr obj))]
   [(list? obj)
    (map extract obj)]
   [else obj]))

(define (main args)
  (with-input-from-file (cadr args)
    (lambda ()
      (let ([obj (extract (read))])
        (call-with-port (open-file-output-port (caddr args) (file-options no-fail) (buffer-mode none))
           (lambda (port)
             (fasl-write obj port)))
        )))


  0)

(main (command-line))
