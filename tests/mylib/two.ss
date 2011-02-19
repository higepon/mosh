(library (two)
  (export <T>-a)
  (import (rnrs))
  (define-record-type <T>
    (fields (mutable a))))
