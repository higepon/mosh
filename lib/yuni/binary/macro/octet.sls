(library (yuni binary macro octet)
         (export pack/octet size/octet unpack/octet)
         (import 
           (rnrs)
           (rnrs base)
                 (rnrs bytevectors))

(define-syntax do-pack/clause
  (syntax-rules ()
    ((_ bv offset val "blob" bytes)
     (bytevector-copy! val 0 bv offset bytes))
    ((_ bv offset #f "pad" bytes)
     (values))
    ((_ bv offset val "single" bytes)
     (bytevector-ieee-single-native-set! bv offset val))
    ((_ bv offset val "single" bytes endian)
     (bytevecotr-ieee-single-set! bv offset val (endianness endian)))
    ((_ bv offset val "double" bytes)
     (bytevector-ieee-double-native-set! bv offset val))
    ((_ bv offset val "double" bytes endian)
     (bytevector-ieee-double-set! bv offset val (endianness endian)))
    ((_ bv offset val "signed" 1)
     (bytevector-s8-set! bv offset val))
    ((_ bv offset val "signed" 2)
     (bytevector-s16-native-set! bv offset val))
    ((_ bv offset val "signed" 4)
     (bytevector-s32-native-set! bv offset val))
    ((_ bv offset val "signed" 8)
     (bytevector-s64-native-set! bv offset val))
    ((_ bv offset val "signed" 2 endian)
     (bytevector-s16-set! bv offset val (endianness endian)))
    ((_ bv offset val "signed" 4 endian)
     (bytevector-s32-set! bv offset val (endianness endian)))
    ((_ bv offset val "signed" 8 endian)
     (bytevector-s64-set! bv offset val (endianness endian)))
    ((_ bv offset val "signed" bytes) ;; else
     (bytevector-sint-set! bv offset val (native-endianness) bytes))
    ((_ bv offset val "signed" bytes endian)
     (bytevector-sint-set! bv offset val (endianness endian) bytes))
    ((_ bv offset val "unsigned" 1)
     (bytevector-u8-set! bv offset val))
    ((_ bv offset val "unsigned" 2)
     (bytevector-u16-native-set! bv offset val))
    ((_ bv offset val "unsigned" 4)
     (bytevector-u32-native-set! bv offset val))
    ((_ bv offset val "unsigned" 8)
     (bytevector-u64-native-set! bv offset val))
    ((_ bv offset val "unsigned" 2 endian)
     (bytevector-u16-set! bv offset val (endianness endian)))
    ((_ bv offset val "unsigned" 4 endian)
     (bytevector-u32-set! bv offset val (endianness endian)))
    ((_ bv offset val "unsigned" 8 endian)
     (bytevector-u64-set! bv offset val (endianness endian)))
    ((_ bv offset val "unsigned" bytes) ;; else
     (bytevector-uint-set! bv offset val (native-endianness) bytes))
    ((_ bv offset val "unsigned" bytes endian)
     (bytevector-uint-set! bv offset val (endianness endian) bytes))))

(define-syntax pack/clause
  (syntax-rules ()
    ((_ bv offset clause)
     (do-pack/clause bv offset . clause))
    ((_ bv offset (pack type size . endian?) rest ...)
     (begin
       (do-pack/clause bv offset . (pack type size . endian?))
       (pack/clause bv (+ offset size) rest ...)))))

(define-syntax lambda-filter-false
  (syntax-rules ()
    ((_ (cur ...) () body ...)
     (lambda (cur ...) body ...))
    ((_ (cur ...) (#f . rest) body ...)
     (lambda-filter-false (cur ...) rest body ...))
    ((_ (cur ...) (head . rest) body ...)
     (lambda-filter-false (cur ... head) rest body ...))))

(define-syntax pack/octet/normalized
  (syntax-rules ()
    ((_ (entry-name . clause) ...)
     (lambda-filter-false () (bv offset entry-name ...)
       (pack/clause bv offset (entry-name . clause) ...)))))

(define-syntax clause-size
  (syntax-rules ()
    ((_ (name type size . endian?))
     size)))

(define-syntax normalize-pack-clause
  (syntax-rules ()
    ((_ ("double" endianness ...))
     (entry-name "double" 8 endianness ...))
    ((_ ("single" endianness ...))
     (entry-name "single" 4 endianness ...))
    ((_ ("bytevector" bytes))
     (entry-name "blob" bytes))
    ((_ ("pad" bytes))
     (#f "pad" bytes))
    ((_ (others ...))
     (entry-name others ...))))

(define-syntax itr-with-normalized-clause
  (syntax-rules ()
    ((_ cns (cur ...) (("double" endianness ...) . rest))
     (itr-with-normalized-clause
       cns
       (cur ... (entry-name "double" 8 endianness ...))
       rest))
    ((_ cns (cur ...) (("single" endianness ...) . rest))
     (itr-with-normalized-clause
       cns
       (cur ... (entry-name "single" 4 endianness ...))
       rest))
    ((_ cns (cur ...) (("bytevector" bytes) . rest))
     (itr-with-normalized-clause
       cns
       (cur ... (entry-name "blob" bytes))
       rest))
    ((_ cns (cur ...) (("pad" bytes) . rest))
     (itr-with-normalized-clause
       cns
       (cur ... (#f "pad" bytes))
       rest))
    ((_ cns (cur ...) ((others ...) . rest))
     (itr-with-normalized-clause
       cns
       (cur ... (entry-name others ...))
       rest))
    ((_ cns (cur ...) ())
     (cns cur ...))))

(define-syntax with-normalized-clause
  (syntax-rules ()
    ((_ cns param ...)
     (itr-with-normalized-clause cns () (param ...)))))

(define-syntax pack/octet
  (syntax-rules ()
    ((_ clause0 ...)
     (with-normalized-clause pack/octet/normalized clause0 ...))))

(define-syntax size/octet/normalized
  (syntax-rules ()
    ((_ clause0 ...)
     (+ (clause-size clause0) ...))))

(define-syntax size/octet
  (syntax-rules ()
    ((_ clause0 ...)
     (with-normalized-clause size/octet/normalized clause0 ...))))

(define (subbytevector bv off l)
  (let ((buf (make-bytevector l)))
    (bytevector-copy! bv off buf 0 l)
    buf))

(define-syntax unpack/clause
  (syntax-rules ()
    ((_ bv offset "blob" bytes)
     (subbytevector bv offset bytes))
    ((_ bv offset "single" bytes)
     (bytevector-ieee-single-native-ref bv offset ))
    ((_ bv offset "single" bytes endian)
     (bytevecotr-ieee-single-ref bv offset (endianness endian)))
    ((_ bv offset "double" bytes)
     (bytevector-ieee-double-native-ref bv offset val))
    ((_ bv offset "double" bytes endian)
     (bytevector-ieee-double-ref bv offset (endianness endian)))
    ((_ bv offset "signed" 1)
     (bytevector-s8-ref bv offset))
    ((_ bv offset "signed" 2)
     (bytevector-s16-native-ref bv offset))
    ((_ bv offset "signed" 4)
     (bytevector-s32-native-ref bv offset))
    ((_ bv offset "signed" 8)
     (bytevector-s64-native-ref bv offset))
    ((_ bv offset "signed" 2 endian)
     (bytevector-s16-ref bv offset (endianness endian)))
    ((_ bv offset "signed" 4 endian)
     (bytevector-s32-ref bv offset (endianness endian)))
    ((_ bv offset "signed" 8 endian)
     (bytevector-s64-ref bv offset (endianness endian)))
    ((_ bv offset "signed" bytes) ;; else
     (bytevector-sint-ref bv offset (native-endianness) bytes))
    ((_ bv offset "signed" bytes endian)
     (bytevector-sint-ref bv offset (endianness endian) bytes))
    ((_ bv offset "unsigned" 1)
     (bytevector-u8-ref bv offset))
    ((_ bv offset "unsigned" 2)
     (bytevector-u16-native-ref bv offset))
    ((_ bv offset "unsigned" 4)
     (bytevector-u32-native-ref bv offset))
    ((_ bv offset "unsigned" 8)
     (bytevector-u64-native-ref bv offset))
    ((_ bv offset "unsigned" 2 endian)
     (bytevector-u16-ref bv offset (endianness endian)))
    ((_ bv offset "unsigned" 4 endian)
     (bytevector-u32-ref bv offset (endianness endian)))
    ((_ bv offset "unsigned" 8 endian)
     (bytevector-u64-ref bv offset (endianness endian)))
    ((_ bv offset "unsigned" bytes) ;; else
     (bytevector-uint-ref bv offset bytes (native-endianness) bytes))
    ((_ bv offset "unsigned" bytes endian)
     (bytevector-uint-ref bv offset bytes (endianness endian) bytes))))

(define-syntax itr-unpack-clause
  (syntax-rules ()
    ((_ (cur ...) bv offset ())
     (values cur ...))
    ((_ cur bv offset ((#f value ...) . rest))
     (itr-unpack-clause cur bv offset rest))
    ((_ (cur ...) bv offset ((name head size endian? ...) . rest))
     (itr-unpack-clause
       (cur ... (unpack/clause bv offset . (head size endian? ...)))
       bv
       (+ offset size)
       rest))))

(define-syntax unpack/octet/normalized
  (syntax-rules ()
    ((_ clause0 ...)
     (lambda (bv offset)
       (itr-unpack-clause () bv offset (clause0 ...))))))

(define-syntax unpack/octet
  (syntax-rules ()
    ((_ clause0 ...)
     (with-normalized-clause unpack/octet/normalized clause0 ...))))

)
