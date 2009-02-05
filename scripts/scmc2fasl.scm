(import (rnrs)
        (srfi :1)
        (mosh))

(define (extract obj)
  (cond
   [(vector? obj)
    (vector-map extract obj)]
   [(and (pair? obj) (eq? (car obj) '*insn*))
    (make-instruction (second obj))]
   [(and (pair? obj) (eq? (car obj) '*compiler-insn*))
    (make-compiler-instruction (second obj))]
   [(list? obj)
    (map extract obj)]
   [else obj]))

(define (main args)
  (with-input-from-file (second args)
    (lambda ()
      (call-with-port (open-file-output-port (third args))
         (lambda (port)
           (fasl-write (extract (read)) port)))))
  0)

(main (command-line))
