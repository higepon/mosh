;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a42/eager-comprehensions.sls
(library (srfi :42)
         (export
             :generator-proc
             dispatch-union
             make-initial-:-dispatch
             :-dispatch-set!
             :-dispatch-ref
             :until
             :while
             :parallel
             :let
             :do
             :dispatched
             :port
             :char-range
             :real-range
             :range
             :integers
             :vector
             :string
             :list
             :
             fold3-ec
             fold-ec
             last-ec
             first-ec
             every?-ec
             any?-ec
             max-ec
             min-ec
             product-ec
             sum-ec
             vector-of-length-ec
             vector-ec
             string-append-ec
             string-ec
             append-ec
             list-ec
             do-ec
         )
         (import
             (srfi :42 eager-comprehensions)
         )
) ;; library (srfi :42)
