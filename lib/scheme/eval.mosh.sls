;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/eval.sls
(library (scheme eval)
         (export
             scheme-report-environment
             null-environment
             eval
             environment
         )
         (import
             (r7b-impl eval)
         )
) ;; library (scheme eval)
