(library (srfi private include compat)
	 (export search-paths)
	 (import (rnrs base)
		 (primitives prefix-list)) ; defined at mosh-utils5.scm
(define (search-paths) prefix-list)
)

