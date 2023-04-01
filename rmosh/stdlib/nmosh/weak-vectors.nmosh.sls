(library (nmosh weak-vectors)
         (export make-weak-vector
                 weak-vector?
                 weak-vector-length
                 weak-vector-ref
                 weak-vector-set!)
         (import (rnrs)
                 (yuni core)
                 (mosh ffi)
                 (primitives object->pointer
                             pointer->object)
                 (prefix (nmosh stubs boehmgc-stubs) stub:))

(define (null-pointer? p)
  (= 0 (pointer->integer p)))
(define* weak-vector (idx-max pointer))

(define (weak-vector? obj)
  (is-a? obj weak-vector))

(define (make-weak-vector size)
  (unless (and (integer? size)
               (positive? size))
    (assertion-violation 'make-weak-vector
                         "invalid argument"
                         size))
  (make weak-vector 
        (pointer (stub:create_weak_vector size))
        (size size)))

(define* (weak-vector-length (wv weak-vector))
  (let-with wv (size) size))

(define-syntax index-check
  (syntax-rules ()
    ((_ name idx size)
     (begin
       (unless (integer? idx)
         (assertion-violation 'name
                              "invalid argument"
                              idx))
       (unless (and (positive? idx)
                    (< idx size))
         (assertion-violation 'name
                              "index out of range"
                              idx))))))

(define* (weak-vector-ref (wv weak-vector) idx)
  (let-with wv (pointer size)
    (index-check weak-vector-ref idx size)
    (let ((p (stub:weak_vector_ref pointer idx)))
      (if (null-pointer? p)
        #f
        (pointer->object p)))))

(define* (weak-vector-set! (wv weak-vector) idx obj)
  (let-with wv (pointer size)
    (index-check weak-vector-set! idx size)
    (let ((p (object->pointer obj)))
      (stub:weak_vector_set pointer idx p)
      (stub:register_disappearing_link_wv pointer idx p))))

)
