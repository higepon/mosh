(library (nmosh conditions)
	 (export &syntax-trace-condition
		 make-syntax-trace-condition
		 syntax-trace-condition?
		 condition-syntax-trace)
	 (import (rnrs))

; NMOSH specific, syntax trace condition
(define-condition-type
  &syntax-trace-condition
  &condition
  make-syntax-trace-condition
  syntax-trace-condition?
  (trace condition-syntax-trace))
)
