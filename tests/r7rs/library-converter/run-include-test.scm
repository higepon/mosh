(import (include-test foo)
        (mosh test))

(test-equal "foo!" foo-var)
(test-results)