(library (testlib)
	 (export foo)
	 (import (rnrs))
(define foo 10))

(import (rnrs) (except (testlib) pitfall))

foo
