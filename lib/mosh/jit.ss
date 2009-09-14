(library (mosh jit)
  (export compile)
  (import (rnrs) (mosh)
          (match)
          (only (srfi private include) include/resolve))

  (include/resolve ("mosh" "jit") "jit-impl.ss")
)
