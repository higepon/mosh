;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/inexact.sls
(library (scheme inexact)
         (export
             tan
             sqrt
             sin
             nan?
             log
             infinite?
             finite?
             exp
             cos
             atan
             asin
             acos
         )
         (import
             (r7b-impl inexact)
         )
) ;; library (scheme inexact)
