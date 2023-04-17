
(import (rnrs))
(import (mosh))
(import (mosh test))

(test-false (rxmatch #/123/ "12"))
(test-true (if (rxmatch #/123/ "123") #t #f))

(test-true (regexp? #/abc/))
(test-false (regexp? "abc"))
(test-equal #/abc/ (string->regexp "abc"))

(test-false (#/123/ "12"))
(test-true (if (#/^abc/ "abc") #t #f))
(test-results)
