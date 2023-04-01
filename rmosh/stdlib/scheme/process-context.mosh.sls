;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/process-context.sls
(library (scheme process-context)
         (export
             get-environment-variables
             get-environment-variable
             exit
             emergency-exit
             command-line
         )
         (import
             (r7b-impl process-context)
         )
) ;; library (scheme process-context)
