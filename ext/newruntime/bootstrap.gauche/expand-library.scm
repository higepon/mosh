(load "./bootstrap.gauche/compat-gauche.scm")
(load "./bootstrap.gauche/mosh-stubs.scm")
(load "./mosh-preload5.scm")
(load "./runtime.scm")
(load "./runtime-cache.scm")
(load "./expander.scm")
(load "./r6rs.gexp")
(load "./system.gexp")

(define (main args)
  (cond
    ((= 3 (length  args))
     (ex:expand-file (cadr args) (caddr args))
     0)
    (else
      -1)))
