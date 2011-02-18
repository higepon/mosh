(library (nmosh ui deco)
         (export deco
                 deco/err
                 decol
                 decol/err)
         (import (rnrs)
                 (srfi :8)
                 (shorten)
                 (yuni text decorate sexp)
                 (nmosh stubs terminal))
(define stderr-tty? '())
(define stdout-tty? '())

(define (isatty? fd)
  (if (= 0 (terminal_isatty fd)) #f #t))

(define (check-tty! sym)
  (case sym
    ((stdout)
     (when (null? stdout-tty?)
       (set! stdout-tty? (isatty? 1))))
    ((stderr)
     (when (null? stderr-tty?)
       (set! stderr-tty? (isatty? 2))))))

(define (sexp->string l)
  (receive (port proc) (open-string-output-port)
    (call-with-port port
                    (^p (write l p) (proc)))))

(define (disp-deco l p)
  (for-each (^e (put-string p e))
            (decorate-sexp-string-list/ecma48 (list (sexp->string l)))))

(define (deco/err l)
  (check-tty! 'stderr)
  (cond
    (stderr-tty? (disp-deco l (current-error-port)))
    (else (write l (current-error-port)))))

(define (deco l)
  (check-tty! 'stdout)
  (cond
    (stdout-tty? (disp-deco l (current-output-port)))
    (else (write l (current-output-port)))))

(define (decol l)
  (deco l)
  (newline (current-output-port)))

(define (decol/err l)
  (deco/err l)
  (newline (current-error-port)))

)
