(define-library (r7b-impl vector)
  (export make-vector vector vector-unfold
          vector-unfold-right vector-copy vector-reverse-copy
          vector-append vector-concatenate vector-append-subvectors
          vector? vector-empty? vector=
          vector-ref vector-length
          vector-fold vector-fold-right vector-map
          vector-map! vector-for-each vector-count
          vector-cumulate
          vector-index vector-index-right vector-skip
          vector-skip-right vector-binary-search vector-any
          vector-every vector-partition
          vector-set! vector-swap! vector-fill!
          vector-reverse! vector-copy! vector-reverse-copy!
          vector-unfold! vector-unfold-right!
          vector->list reverse-vector->list list->vector
          reverse-list->vector vector->string string->vector)
  (import (scheme cxr)
          (except (scheme base) list->vector string->vector vector-append vector-copy vector-copy! vector-fill! vector-for-each vector->list vector-map vector->string))
  (include "vector-impl.scm"))
