(library (one)
  (export make-<T>)
  (import (rnrs))
  (define-record-type <T>
    (nongenerative the-t)
    (fields (mutable a))))
