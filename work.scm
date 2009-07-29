(import (rnrs)
        (srfi :2 and-let*))

(and-let* ((entry (assoc 'hige '()))) (cdr entry))
