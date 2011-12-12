(library (nmosh pffi posix ffithread)
         (export ffithread-invoke)
         (import (rnrs)
                 (prefix (nmosh stubs posix-ffithread) stub:))

(define ffithread-invoke stub:posix_invoke_ffithread)

)
