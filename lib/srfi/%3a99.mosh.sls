;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a99/records.mosh.sls
(library (srfi :99)
         (export
             define-record-type
             record?
             record-rtd
             rtd-name
             rtd-parent
             rtd-field-names
             rtd-all-field-names
             rtd-field-mutable?
             make-rtd
             rtd?
             rtd-constructor
             rtd-predicate
             rtd-accessor
             rtd-mutator
         )
         (import
             (srfi :99 records)
         )
) ;; library (srfi :99)
