;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/char/normalization.sls
(library (scheme char normalization)
         (export
             string-ni>?
             string-ni>=?
             string-ni=?
             string-ni<?
             string-ni<=?
         )
         (import
             (r7b-impl char normalization)
         )
) ;; library (scheme char normalization)
