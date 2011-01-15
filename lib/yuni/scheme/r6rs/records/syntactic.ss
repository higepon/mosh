(library (yuni scheme r6rs records syntactic)

  (export define-record-type fields mutable immutable
          parent protocol sealed opaque nongenerative parent-rtd
          record-type-descriptor record-constructor-descriptor)

  (import 
    (yuni scheme r6rs))
) ; rnrs records syntactic


