(import (rnrs)
        (srfi :2 and-let*))

(display (and-let* ((entry (assoc 'hige '()))) (cdr entry)))
