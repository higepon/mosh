(library (yuni binary macro packet0)
         (export
           define-packet0*)
         (import
           (rnrs)
           (yuni core)
           (yuni binary macro octet))

(define-syntax define-packet0*-packer
  (syntax-rules ()
    ((_ name PACK (names ...) (clause ...))
     (define (PACK input)
       (let-with input (names ...)
         (define bv (make-bytevector (size/octet clause ...)))
         (pack!/octet bv 0 clause ...)
         bv
         )))))

(define-syntax define-packet0*-unpacker
  (syntax-rules ()
    ((_ name UNPACK (names ...) (clause ...))
     (define (UNPACK input offset)
       (unpack-let/octet input offset (clause ...)
                         (make name
                               (names names) ...))))))

(define-syntax define-packet0*-itr
  (syntax-rules ()
    ((_ name PACK UNPACK (names ...) (clause ...) ())
     (begin
       (define* name (names ...))
       (define-packet0*-packer name PACK (names ...) (clause ...))
       (define-packet0*-unpacker name UNPACK (names ...) (clause ...))))
    ((_ name PACK UNPACK (names ...) (clausep ...) 
        ((cname crest ...) clause1 ...))
     (define-packet0*-itr
       name PACK UNPACK (names ... cname) 
       (clausep ... (cname crest ...))
       (clause1 ...)))))

(define-syntax define-packet0*
  (syntax-rules ()
    ((_ name PACK UNPACK clause ...)
     (define-packet0*-itr
       name PACK UNPACK () () (clause ...)))))

)
