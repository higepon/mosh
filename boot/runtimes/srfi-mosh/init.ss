(import (rnrs) (nmosh runlib) (only (mosh) set-symbol-value! symbol-value))

; mosh initialize from psyntax
(set-symbol-value!
  'create-non-continuable-violation
  (lambda (c)
    (condition (make-non-continuable-violation)
	       (make-who-condition 'raise)
	       (make-message-condition "returned from non-continuable exception")
	       (make-irritants-condition (list c)))))

(set-symbol-value! 'trace-printer write)

(unless (guard (c (#t #f)) (symbol-value '%disable-acc))
  (set-symbol-value! '%disable-acc #f))

(cond
  ((guard (c (#t #f)) (symbol-value '%vm-import-spec))
   (runlib (symbol-value '%vm-import-spec) (symbol-value '%vm-thunk)))
  (else 
   (runlib '((nmosh startup)) 'startup)))

