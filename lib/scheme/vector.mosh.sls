;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/vector.sls
(library (scheme vector)
         (export
             string->vector
             vector->string
             reverse-list->vector
             list->vector
             reverse-vector->list
             vector->list
             vector-unfold-right!
             vector-unfold!
             vector-reverse-copy!
             vector-copy!
             vector-reverse!
             vector-fill!
             vector-swap!
             vector-set!
             vector-partition
             vector-every
             vector-any
             vector-binary-search
             vector-skip-right
             vector-skip
             vector-index-right
             vector-index
             vector-cumulate
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
             vector-append-subvectors
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
             (r7b-impl vector)
         )
) ;; library (scheme vector)
