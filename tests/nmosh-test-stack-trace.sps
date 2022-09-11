;; FIXME: This require profiler build to get sourcenames on cprocs
(import (rnrs)
        (mosh test)
        (rnrs load))

(load "./tests/nmosh-testlib-stack-trace.sls")

;; trigger test 
;; (We cannot use test-* construct here, because they catch exceptions..)
(load "./tests/stack-trace-nmosh.sps")
