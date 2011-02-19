(import (rnrs)
        (mosh test)
        (foo bar compat))

(test-eq 'compat-mosh message)
(test-results)
