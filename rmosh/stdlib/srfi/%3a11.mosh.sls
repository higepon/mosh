;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a11/let-values.sls
(library (srfi :11)
         (export
             let*-values
             let-values
         )
         (import
             (srfi :11 let-values)
         )
) ;; library (srfi :11)
