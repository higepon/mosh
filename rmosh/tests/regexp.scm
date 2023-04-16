
(import (rnrs))
(import (mosh))
(import (mosh test))

(test-false (rxmatch #/123/ "12"))

(test-results)
