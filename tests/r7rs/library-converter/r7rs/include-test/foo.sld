
(define-library (include-test foo)
    (export foo-var bar baz)
    (import (scheme base))
    (include "foo.scm")
    (include-ci "bar.scm" "baz.scm")
)