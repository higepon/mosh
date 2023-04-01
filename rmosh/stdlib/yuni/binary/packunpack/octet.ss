(library (yuni binary packunpack octet)
	 (export octet-unpack octet-pack octet-size)
	 (import (rnrs))

(define-syntax octet-size-step
  (syntax-rules (unsigned signed pad)
    ((_ (unsigned X bogus)) X)
    ((_ (signed X bogus)) X)
    ((_ (pad X)) X)))

(define-syntax octet-size
  (syntax-rules ()
    ((_ (pack0 pack1 ...))
     (+ (octet-size-step pack0) (octet-size (pack1 ...))))
    ((_ ()) 0)))

(define little-endian?
  (eq? (native-endianness) 'little))


(define bytevector-uint-set!/native
  (if little-endian?
    (lambda (bv off param size)
      (bytevector-uint-set! bv off param (endianness little) size))
    (lambda (bv off param size)
      (bytevector-uint-set! bv off param (endianness big) size))))

(define bytevector-sint-set!/native
  (if little-endian?
    (lambda (bv off param size)
      (bytevector-sint-set! bv off param (endianness little) size))
    (lambda (bv off param size)
      (bytevector-sint-set! bv off param (endianness big) size))))

(define-syntax do-single-octet-pack/unaligned
  (syntax-rules (unsigned signed pad)
    ((_ bv off (unsigned X) param)
     (bytevector-uint-set!/native bv off param X))
    ((_ bv off (signed X) param)
     (bytevector-sint-set!/native bv off param X))
    ((_ bv off (unsigned X en) param)
     (bytevector-uint-set! bv off param (endianness en) X))
    ((_ bv off (signed X en) param)
     (bytevector-sint-set! bv off param (endianness en) X))))

(define-syntax do-single-octet-pack
  (syntax-rules ()
    ((_ bv off pack param)
     (do-single-octet-pack/unaligned bv off pack param))))

(define-syntax do-octet-pack
  (syntax-rules (pad)
    ((_ bv off () ()) bv)
    ((_ bv off ((pad X) pack1 ...) (param0 ...))
     ; treat param is 0
     (begin (do-single-octet-pack bv off (unsigned X) 0)
	    (do-octet-pack bv
			   (+ off X)
			   (pack1 ...)
			   (param0 ...))))
    ((_ bv off (pack0 pack1 ...) (param0 param1 ...))
     (begin (do-single-octet-pack bv off pack0 param0)
	    (do-octet-pack bv 
			   (+ off (octet-size (pack0))) 
			   (pack1 ...) 
			   (param1 ...))))))

(define-syntax octet-pack
  (syntax-rules ()
    ((_ bv off pack param)
     (do-octet-pack bv off pack param))
    ((_ pack param)
     (let ((bv (make-bytevector (octet-size pack))))
       (do-octet-pack bv 0 pack param)))))

(define-syntax entry-length
  (syntax-rules (pad)
    ((_ ()) 0)
    ((_ ((pad X) entry0 ...)) (entry-length (entry0 ...)))
    ((_ (entry0 entry1 ...)) (+ 1 (entry-length (entry1 ...))))))

(define-syntax read1/unaligned
  (syntax-rules (unsigned signed pad)
    ((_ bv off (unsigned X en))
     (bytevector-uint-ref bv off (endianness en) X))
    ((_ bv off (signed X en))
     (bytevector-sint-ref bv off (endianness en) X))))

(define-syntax do-single-octet-unpack
  (syntax-rules (pad)
    ((_ vec idx bv off (pad bogus)) (values)) ; do nothing
    ((_ vec idx bv off pack)
     (vector-set! vec idx (read1/unaligned bv off pack)))))

(define-syntax octet-unpack-itr
  (syntax-rules (pad)
    ((_ vec idx bv off ()) vec)
    ((_ vec idx bv off ((pad X) pack0 ...))
     (octet-unpack-itr vec
		       idx
		       bv
		       (+ X off)
		       (pack0 ...)))
    ((_ vec idx bv off (pack0 pack1 ...))
     (begin
       (do-single-octet-unpack vec idx bv off pack0)
       (octet-unpack-itr vec 
			 (+ 1 idx) 
			 bv 
			 (+ (octet-size (pack0)) off) 
			 (pack1 ...))))))

(define-syntax octet-unpack
  (syntax-rules ()
    ((_ bv off pack)
     (let ((vec (make-vector (entry-length pack))))
       (octet-unpack-itr vec 0 bv off pack)))))

)
