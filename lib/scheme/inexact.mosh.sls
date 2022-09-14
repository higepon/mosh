;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/inexact.sls
(library (scheme inexact)
         (export
acos asin
atan cos
exp finite?
infinite? log
nan? sin
sqrt tan
         )
         (import
             (r7b-impl inexact)
         )
) ;; library (scheme inexact)
