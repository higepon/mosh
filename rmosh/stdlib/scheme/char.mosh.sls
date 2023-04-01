;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/char.sls
(library (scheme char)
         (export
             string-upcase
             string-foldcase
             string-downcase
             string-ci>?
             string-ci>=?
             string-ci=?
             string-ci<?
             string-ci<=?
             digit-value
             char-whitespace?
             char-upper-case?
             char-upcase
             char-numeric?
             char-lower-case?
             char-foldcase
             char-downcase
             char-ci>?
             char-ci>=?
             char-ci=?
             char-ci<?
             char-ci<=?
             char-alphabetic?
         )
         (import
             (r7b-impl char)
         )
) ;; library (scheme char)
