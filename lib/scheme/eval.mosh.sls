;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/eval.sls
(library (scheme eval)
         (export
             eval
             environment
         )
         (import
             (r7b-impl eval)
         )
) ;; library (scheme eval)
