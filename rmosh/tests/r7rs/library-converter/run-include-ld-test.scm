(import (baz)
        (scheme base)
        (mosh test))

(test-equal "is fun" (life))
(test-equal 'mosh (name))
(test-results)