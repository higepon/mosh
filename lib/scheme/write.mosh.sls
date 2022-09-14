;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/write.mosh.sls
(library (scheme write)
         (export
             write-simple
             write-shared
             write
             display
         )
         (import
             (r7b-impl write)
         )
) ;; library (scheme write)
