(import (nmosh)
        (nrepl simple io)
        (nrepl simple)
        (mosh test)
	(primitives ex:expand-sequence/debug ex:repl))

;:

;; I/O tests
(test-equal ''hoge (nrepl-read-one (open-string-input-port "(quote\nhoge)")))
(test-error i/o-error? (nrepl-read-one (open-string-input-port "(FAIL")))
(test-error i/o-read-error? (nrepl-read-one (open-string-input-port "(FAIL")))

;; FIXME: no reader test

(test-results)
