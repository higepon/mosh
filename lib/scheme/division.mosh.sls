;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/division.sls
(library (scheme division)
         (export
             truncate/
             truncate-remainder
             truncate-quotient
             round/
             round-remainder
             round-quotient
             floor/
             floor-remainder
             floor-quotient
             euclidean/
             euclidean-remainder
             euclidean-quotient
             centered/
             centered-remainder
             centered-quotient
             ceiling/
             ceiling-remainder
             ceiling-quotient
         )
         (import
             (r7b-impl division)
         )
) ;; library (scheme division)
