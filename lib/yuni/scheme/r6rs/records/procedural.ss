(library (yuni scheme r6rs records procedural)
  (export
   make-record-type-descriptor record-type-descriptor?
   make-record-constructor-descriptor record-constructor
   record-predicate record-accessor record-mutator)
  (import
    (yuni scheme r6rs)))
