;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a37/args-fold.sls
(library (srfi :37)
         (export
             option-processor
             option-optional-arg?
             option-required-arg?
             option-names
             option?
             args-fold
             option
         )
         (import
             (srfi :37 args-fold)
         )
) ;; library (srfi :37)
