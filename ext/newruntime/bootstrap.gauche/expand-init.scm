(load "./bootstrap.gauche/compat-gauche.scm")
(load "./bootstrap.gauche/mosh-stubs.scm")
(load "./runtime.scm")
(load "./runtime-cache.scm")
(load "./expander.scm")
(load "./r6rs.gexp")
(load "./system.gexp")
(load "./repl.gexp")

(define (main args)
  (cond
    ((= 3 (length  args))
     (ex:expand-file (cadr args) (caddr args))
     0)
    (else
      -1)))
