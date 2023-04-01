;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a38/with-shared-structure.mosh.sls
(library (srfi :38)
         (export
             write/ss
             write-with-shared-structure
             read/ss
             read-with-shared-structure
         )
         (import
             (srfi :38 with-shared-structure)
         )
) ;; library (srfi :38)
