#!r6rs
(library (r7b-impl process-context)
         (export
command-line emergency-exit
exit
get-environment-variable
get-environment-variables
)
         (import (rnrs) (srfi i98))

(define emergency-exit exit)
)
