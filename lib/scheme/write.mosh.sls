;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/write.mosh.sls
(library (scheme write)
         (export
             write-shared
             write-simple
             write
             display
         )
         (import
             (r7b-impl write)
         )
) ;; library (scheme write)
