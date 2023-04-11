;; Multiple imports are allowed in R7RS.
(import (scheme base))
(import (scheme write))
(import (mosh test))
(import (only (srfi 1) first second))

;; feature based cond-expand.
(cond-expand
 [mosh #t]
 [else (error "This should not be executed")])

 ;; include
 (include "include/a.scm" "include/b.scm")
 (test-equal 'a a)
 (test-equal 'b b)

(cond-expand
  [(library (mosh)) #t]
  [else (error "This should not be executed")])

 (define my-name (cond-expand (gosh 'gosh) (mosh 'mosh) (else 'unknown)))
 (test-equal 'mosh my-name)

 (test-results)