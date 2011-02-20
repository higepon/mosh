;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a43/vectors.sls
(library (srfi :43)
         (export
             reverse-list->vector
             list->vector
             reverse-vector->list
             vector->list
             vector-reverse-copy!
             vector-copy!
             vector-reverse!
             vector-fill!
             vector-swap!
             vector-set!
             vector-every
             vector-any
             vector-binary-search
             vector-skip-right
             vector-index-right
             vector-skip
             vector-index
             vector-count
             vector-for-each
             vector-map!
             vector-map
             vector-fold-right
             vector-fold
             vector-length
             vector-ref
             vector=
             vector-empty?
             vector?
             vector-concatenate
             vector-append
             vector-reverse-copy
             vector-copy
             vector-unfold-right
             vector-unfold
             vector
             make-vector
         )
         (import
             (srfi :43 vectors)
         )
) ;; library (srfi :43)
