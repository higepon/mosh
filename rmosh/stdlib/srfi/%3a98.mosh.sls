;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a98/os-environment-variables.sls
(library (srfi :98)
         (export
             get-environment-variables
             get-environment-variable
         )
         (import
             (srfi :98 os-environment-variables)
         )
) ;; library (srfi :98)
