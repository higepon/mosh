(library (yuni binary packunpack bit)
	 (export bit-pack-word!
		 bit-unpack-word!)
	 (import (rnrs))

(define-syntax param-size
  (syntax-rules (pad signed unsigned boolean)
    ((_ (pad X)) 0)
    ((_ (signed X)) 1)
    ((_ (unsigned X)) 1)
    ((_ (boolean)) 1)))

(define-syntax pack-ref
  (syntax-rules (signed unsigned boolean)
    ((_ v voff (unsigned X))
     (bitwise-bit-field v voff (+ voff X)))
    ((_ v voff (signed X))
     (let ((b (pack-ref v voff (unsigned X))))
       (if (bitwise-bit-set? b (- X 1))
	 (- b (bit-loc X))
	 b)))
    ((_ v voff (boolean))
     (bitwise-bit-set? v voff))))

(define-syntax do-bit-unpack-word!-1
  (syntax-rules (pad)
    ((_ vec off v voff (pad X)) 'ok) ; do nothing
    ((_ vec off v voff pack)
     (vector-set! vec off (pack-ref v voff pack)))))

(define-syntax do-bit-unpack-word!
  (syntax-rules ()
    ((_ vec off v voff (pack0))
     (do-bit-unpack-word!-1 vec off v voff pack0))
    ((_ vec off v voff (pack0 pack1 ...))
     (begin
       (do-bit-unpack-word!-1 vec off v voff pack0)
       (do-bit-unpack-word! vec (+ off (param-size pack0)) v (+ voff (calc-bitwidth pack0)) (pack1 ...))))))

(define-syntax bit-unpack-word!
  (syntax-rules ()
    ((_ bv off size en vec voff pack)
     (let ((h (bytevector-uint-ref bv off (endianness en) size)))
       (do-bit-unpack-word! vec voff h 0 pack)))))

(define-syntax calc-bitwidth
  (syntax-rules (boolean signed unsigned pad)
    ((_ (boolean)) 1)
    ((_ (signed X)) X)
    ((_ (unsigned X)) X)
    ((_ (pad X)) X)))

(define-syntax bit-loc
  (syntax-rules ()
    ((_ X) (expt 2 X))))

(define-syntax signed-bits
  (syntax-rules ()
    ((_ X v)
     (if (< v 0) (+ (bit-loc X) v) v))))

(define-syntax do-pack-set
  (syntax-rules ()
    ((_ bitpos len v)
     (* (bit-loc bitpos) v)))) ; FIXME: do bit-mask!

(define-syntax do-pack-word-1
  (syntax-rules (boolean signed unsigned)
    ((_ bitpos (boolean) param0)
     (do-pack-set bitpos 1 (if param0 1 0)))
    ((_ bitpos (signed X) param0)
     (do-pack-set bitpos X (signed-bits X param0)))
    ((_ bitpos (unsigned X) param0)
     (do-pack-set bitpos X param0))))

(define-syntax do-pack-word-itr
  (syntax-rules ()
    ((_ bitpos (pack0) param paramrest)
     (do-pack-word-1 bitpos pack0 param))
    ((_ bitpos (pack0 pack1 ...) param paramrest)
     (+ (do-pack-word-1 bitpos pack0 param)
	(do-pack-word-consume (+ (calc-bitwidth pack0) bitpos)
			      (pack1 ...)
			      paramrest)))))

(define-syntax do-pack-word-consume
  (syntax-rules (pad)
    ((_ bitpos ((pad X) pack0 ...) (param0 ...))
     (do-pack-word-itr bitpos ((unsigned X) pack0 ...) 0 (param0 ...)))
    ((_ bitpos (pack0 ...) (param0 param1 ...))
     (do-pack-word-itr bitpos (pack0 ...) param0 (param1 ...)))))

(define-syntax do-pack-word
  (syntax-rules ()
    ((_ pack param)
     (do-pack-word-consume 0 pack param))))

(define-syntax bit-pack-word!
  (syntax-rules ()
    ((_ bv off size en pack param)
     (bytevector-uint-set! bv off 
			   (do-pack-word pack param) 
			   (endianness en) size))))

)
