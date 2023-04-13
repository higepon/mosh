(import (include-test foo)
        (mosh test))

(test-equal "foo!" foo-var)
(test-equal "bar" bar)
(test-equal "baz" baz)
(test-results)