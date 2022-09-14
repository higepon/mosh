;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/lazy.sls
(library (scheme lazy)
         (export
delay delay-force
force make-promise
promise?
         )
         (import
             (r7b-impl lazy)
         )
) ;; library (scheme lazy)
