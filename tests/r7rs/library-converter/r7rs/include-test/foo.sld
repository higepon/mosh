
(define-library (include-test foo)
    (export foo-var)
    (import (scheme base))
    (include "foo.scm"))