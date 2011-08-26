(library (nmosh bootstrap vm-apply)
         (export vm/apply)
         (import (rnrs))

(define (vm/apply . x)
  (assertion-violation 'vm/apply "unimplemented" x))
)
