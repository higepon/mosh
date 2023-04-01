(library (yuni binary macro bitwise)
         (export
           pack/bitwise
           unpack-let/bitwise)
         (import (rnrs))

(define (calc from to val)
  (bitwise-copy-bit-field 0 from to val))

(define-syntax return-pack/bitwise
  (syntax-rules ()
    ((_ (form0 ...))
     (bitwise-ior form0 ...))))

(define-syntax itr-pack/bitwise
  (syntax-rules ()
    ((_ acc shift) (return-pack/bitwise acc))
    ((_ acc shift ("pad" width) rest ...)
     (itr-pack/bitwise acc (+ width shift) rest ...))
    ((_ acc shift (id "unsigned" width) rest ...)
     (let ((next (+ shift width)))
       (itr-pack/bitwise
         ((calc shift next id) . acc)
         next rest ...)))
    ((_ acc shift (id "signed" width) rest ...)
     (let ((val (if (> 0 id) (+ 1 (bitwise-not (- 0 id))) id))
           (next (+ shift width)))
       (itr-pack/bitwise
         ((calc shift next val) . acc)
         next rest ...)))))

(define-syntax pack/bitwise
  (syntax-rules ()
    ((_ clause0 ...)
     (itr-pack/bitwise () 0 clause0 ...))))

(define (unsigned->signed width x) ;; sign-extend x
  (if (bitwise-bit-set? x (- width 1))
    (bitwise-ior (bitwise-arithmetic-shift-left -1 width)
                 x)
    x))

(define-syntax itr-unpack-let/bitwise
  (syntax-rules ()
    ((_ v offset () body) body)
    ((_ v offset ((id "signed" width) . rest) body)
     (let ((next (+ width offset)))
       (itr-unpack-let/bitwise 
         v next rest
         (let ((id (unsigned->signed
                     width
                     (bitwise-bit-field v offset next))))
           body))))
    ((_ v offset ((id "unsigned" width) . rest) body)
     (let ((next (+ width offset)))
       (itr-unpack-let/bitwise 
         v next rest
         (let ((id (bitwise-bit-field v offset next)))
           body))))
    ((_ v offset (("pad" width) . rest) body)
     (itr-unpack-let/bitwise v (+ width offset) rest body))))

(define-syntax unpack-let/bitwise
  (syntax-rules ()
    ((_ v (clause0 ...) body ...)
     (itr-unpack-let/bitwise v 0 (clause0 ...) (let () body ...)))))

)
