(library (yuni text makefile recipe)
         (export recipe
                 recipe?
                 recipe-phony?
                 recipe-target
                 recipe-deps
                 recipe-commands)
         (import (yuni core)
                 (rnrs))
(define* recipe 
  (phony? target dep* cmd*))

(define (recipe? r)
  (is-a? r recipe))

(define* (recipe-phony? (recipe))
  (let-with recipe (phony?) phony?))

(define* (recipe-target (recipe))
  (let-with recipe (target) target))

(define* (recipe-deps (recipe))
  (let-with recipe (dep*) dep*))

(define* (recipe-commands (recipe))
  (let-with recipe (cmd*) cmd*))

)
