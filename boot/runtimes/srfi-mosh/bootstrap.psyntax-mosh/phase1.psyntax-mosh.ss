(import 
  (rnrs)
  (rnrs eval)
  (only (psyntax system $all) interaction-environment)
  (mosh))

(define m (interaction-environment))

(define (read-all/port p)
  (let ((r (read p)))
    (if (eof-object? r)
      '()
      (cons r (read-all/port p)))))

(define (read-all fn)
  (call-with-input-file fn read-all/port))

(define (load-file fn)
  (load-list (read-all fn)))
(define (load-fasl fn)
  (load-list (call-with-port (open-file-input-port fn) fasl-read)))

(define (load-list l)
  (for-each (lambda (e) (eval e m)) l))

(load-file "bootstrap.psyntax-mosh/phase1.psyntax-mosh-CODE0.ss")
(load-file "compat-mosh-run.scm")
(load-file "runtime.scm")
(load-file "mosh-utils5.scm")
(load-fasl "BOOT0.fasl")
(load-file "bootstrap.psyntax-mosh/phase1.psyntax-mosh-CODE1.ss")


