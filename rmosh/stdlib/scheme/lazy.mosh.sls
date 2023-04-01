;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/lazy.sls
(library (scheme lazy)
         (export
             promise?
             make-promise
             force
             delay-force
             delay
         )
         (import
             (r7b-impl lazy)
         )
) ;; library (scheme lazy)
