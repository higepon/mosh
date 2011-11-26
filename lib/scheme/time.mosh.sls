;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/time.sls
(library (scheme time)
         (export
             jiffies-per-second
             current-second
             current-jiffy
         )
         (import
             (r7b-impl time)
         )
) ;; library (scheme time)
