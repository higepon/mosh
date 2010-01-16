(library (core let-values)
  (export let-values let*-values)
  (import (for (core primitives)   expand run)
          (for (core syntax-rules) expand)
          (core let)
          (primitives call-with-values))

  (define-syntax let-values
    (syntax-rules ()
      ((let-values (?binding ...) ?body0 ?body1 ...)
       (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
      ((let-values "bind" () ?tmps ?body)
       (let ?tmps ?body))
      ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
       (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
      ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
       (call-with-values
        (lambda () ?e0)
        (lambda ?args
          (let-values "bind" ?bindings ?tmps ?body))))
      ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
      ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (call-with-values
        (lambda () ?e0)
        (lambda (?arg ... . x)
          (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () ?body0 ?body1 ...)
       (begin ?body0 ?body1 ...))
      ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
       (let-values (?binding0)
         (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

  ) ; core let-values
