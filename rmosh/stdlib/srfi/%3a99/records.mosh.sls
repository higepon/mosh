;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a99/records/procedural.sls
;;   lib/srfi/%3a99/records/inspection.sls
;;   lib/srfi/%3a99/records/syntactic.sls
(library (srfi :99 records)
         (export
             rtd-mutator
             rtd-accessor
             rtd-predicate
             rtd-constructor
             rtd?
             make-rtd
             rtd-field-mutable?
             rtd-all-field-names
             rtd-field-names
             rtd-parent
             rtd-name
             record-rtd
             record?
             define-record-type
         )
         (import
             (srfi :99 records procedural)
             (srfi :99 records inspection)
             (srfi :99 records syntactic)
         )
) ;; library (srfi :99 records)
