(library (test0)
	 (export foo)
	 (import (rnrs))
(define foo 10))
(library (test1)
	 (export boo)
	 (import (test0) (rnrs))
(define boo foo))

(import (rnrs) (test1))

boo
