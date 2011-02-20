;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a27/random-bits.sls
(library (srfi :27)
         (export
             random-source-make-reals
             random-source-make-integers
             random-source-pseudo-randomize!
             random-source-randomize!
             random-source-state-set!
             random-source-state-ref
             random-source?
             make-random-source
             default-random-source
             random-real
             random-integer
         )
         (import
             (srfi :27 random-bits)
         )
) ;; library (srfi :27)
