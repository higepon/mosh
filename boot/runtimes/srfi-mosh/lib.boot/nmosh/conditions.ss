(library (nmosh conditions)
	 (export &syntax-trace-condition
		 make-syntax-trace-condition
		 syntax-trace-condition?
		 condition-syntax-trace
                 condition-syntax-form
                 condition-syntax-subform)
	 (import (rnrs))

; NMOSH specific, syntax trace condition
(define-condition-type
  &syntax-trace-condition
  &condition
  make-syntax-trace-condition
  syntax-trace-condition?
  (syntax-form condition-syntax-form)
  (syntax-subform condition-syntax-subform)
  (trace condition-syntax-trace))
)
