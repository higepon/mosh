;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/complex.sls
(library (scheme complex)
         (export
             real-part
             make-rectangular
             make-polar
             magnitude
             imag-part
             angle
         )
         (import
             (r7b-impl complex)
         )
) ;; library (scheme complex)
