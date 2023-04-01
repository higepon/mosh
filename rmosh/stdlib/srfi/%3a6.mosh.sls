;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a6/basic-string-ports.sls
(library (srfi :6)
         (export
             get-output-string
             open-output-string
             open-input-string
         )
         (import
             (srfi :6 basic-string-ports)
         )
) ;; library (srfi :6)
