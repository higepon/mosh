(library (yuni binary macro octet)
         (export pack!/octet size/octet unpack-let/octet)
         (import 
           (rnrs)
           (rnrs base)
                 (rnrs bytevectors))

(define-syntax do-pack/clause
  (syntax-rules ()
    ((_ bv offset val "blob" bytes from-offset)
     (bytevector-copy! val from-offset bv offset bytes))
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

(define-syntax pack!/octet/normalized
  (syntax-rules ()
    ((_ (bv offset) clause ...)
     (let ()
       (pack/clause bv offset clause ...)))))

(define-syntax clause-size
  (syntax-rules ()
    ((_ (name type size . endian?))
     size)))

(define-syntax itr-with-normalized-clause
  (syntax-rules ()
    ((_ param cns (cur ...) ((id "double" endianness ...) . rest))
     (itr-with-normalized-clause
       param
       cns
       (cur ... (id "double" 8 endianness ...))
       rest))
    ((_ param cns (cur ...) ((id "single" endianness ...) . rest))
     (itr-with-normalized-clause
       param
       cns
       (cur ... (id "single" 4 endianness ...))
       rest))
    ((_ param cns (cur ...) (("pad" bytes) . rest))
     (itr-with-normalized-clause
       param
       cns
       (cur ... (#f "pad" bytes))
       rest))
    ((_ param cns (cur ...) ((id "blob" bytes) . rest)) ;; offset 0
     (itr-with-normalized-clause
       param
       cns
       (cur ... (id "blob" bytes 0))
       rest))
    ((_ param cns (cur ...) ((id "blob!" bytes) . rest)) ;; offset 0
     (itr-with-normalized-clause
       param
       cns
       (cur ... (#f "blob!" bytes id 0 bytes))
       rest))
    ((_ param cns (cur ...) ((id "blob!" bytes offset) . rest)) ;; offset 0
     (itr-with-normalized-clause
       param
       cns
       (cur ... (#f "blob!" bytes id offset bytes))
       rest))
    ((_ param cns (cur ...) ((id "blob!" bytes offset count) . rest))
     (itr-with-normalized-clause
       param
       cns
       (cur ... (#f "blob!" bytes id offset count))
       rest))
    ((_ param cns (cur ...) ((others ...) . rest))
     (itr-with-normalized-clause
       param
       cns
       (cur ... (others ...))
       rest))
    ((_ param cns (cur ...) ())
     (cns param cur ...))))

(define-syntax with-normalized-clause
  (syntax-rules ()
    ((_ param cns clause ...)
     (itr-with-normalized-clause param cns () (clause ...)))))

(define-syntax pack!/octet
  (syntax-rules ()
    ((_ bv offset clause0 ...)
     (with-normalized-clause (bv offset) pack!/octet/normalized clause0 ...))))

(define-syntax size/octet/normalized
  (syntax-rules ()
    ((_ bogus clause0 ...)
     (+ (clause-size clause0) ...))))

(define-syntax size/octet
  (syntax-rules ()
    ((_ clause0 ...)
     (with-normalized-clause #f size/octet/normalized clause0 ...))))

(define (subbytevector bv off l)
  (let ((buf (make-bytevector l)))
    (bytevector-copy! bv off buf 0 l)
    buf))

(define-syntax unpack/clause
  (syntax-rules ()
    ((_ bv offset "blob" bytes bogus)
     (subbytevector bv offset bytes))
    ((_ bv offset "blob!" bytes id dest-offset dest-count)
     (bytevector-copy! bv offset id dest-offset dest-count))
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
    ((_ bv offset (body ...) ())
     (let () body ...))
    ((_ bv offset body ((#f value ...) . rest)) ;; side-effect (or pad)
     (begin
       (unpack/clause bv offset . (value ...))
       (itr-unpack-clause bv offset body rest)))
    ((_ bv offset body ((name head size endian? ...) . rest))
     (let ((name (unpack/clause bv offset . (head size endian? ...))))
       (itr-unpack-clause bv (+ offset size) body rest)))))

(define-syntax unpack-let/octet/normalized
  (syntax-rules ()
    ((_ (bv offset body ...) clause0 ...)
     (let ()
       (itr-unpack-clause bv offset (body ...) (clause0 ...))))))

(define-syntax unpack-let/octet
  (syntax-rules ()
    ((_ bv offset (clause0 ...))
     (unpack-let/octet bv offset (clause0 ...) (values)))
    ((_ bv offset (clause0 ...) body ...)
     (with-normalized-clause (bv offset body ...) 
                             unpack-let/octet/normalized clause0 ...))))

)
