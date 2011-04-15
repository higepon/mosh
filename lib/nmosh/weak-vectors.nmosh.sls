(library (nmosh weak-vectors)
         (export make-weak-vector
                 weak-vector?
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
        (idx-max (if (= size 0) 
                   0
                   (- size 1)))))

(define* (weak-vector-ref (wv weak-vector) idx)
  (let-with wv (pointer idx-max)
    (unless (<= 0 idx idx-max)
      (assertion-violation 'weak-vector-ref
                           "index out of range"
                           idx))
    (let ((p (stub:weak_vector_ref pointer idx)))
      (if (null-pointer? p)
        #f
        (pointer->object p)))))

(define* (weak-vector-set! (wv weak-vector) idx obj)
  (let-with wv (pointer idx-max)
    (unless (<= 0 idx idx-max)
      (assertion-violation 'weak-vector-ref
                           "index out of range"
                           idx))
    (let ((p (object->pointer obj)))
      (stub:weak_vector_set pointer idx p)
      (stub:register_disappearing_link_wv pointer idx p))))

)
