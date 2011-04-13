(library (nmosh guardian)
         (export make-guardian)
         (import (rnrs)
                 (rnrs mutable-pairs)
                 (nmosh stubs boehmgc-stubs)
                 (mosh ffi)
                 (shorten)
                 (srfi :8)
                 (primitives 
                   bytevector-pointer
                   object->pointer
                   pointer->object))

(define (make-weak-box)
  (make-bytevector size-of-void* #xff))

(define (weak-box-set! box ptr)
  (case size-of-void*
    ((4)
     (bytevector-u32-native-set! box 0 ptr))
    ((8)
     (bytevector-u64-native-set! box 0 ptr))))

(define (weak-box-ref-num box)
  (case size-of-void*
    ((4)
     (bytevector-u32-native-ref box 0))
    ((8)
     (bytevector-u64-native-ref box 0))))

(define (weak-box-ref box)
  (integer->pointer (weak-box-ref-num box)))

(define (weak-box-broken? b)
  (= 0 (bytevector-u8-ref b 0)))

(define (hide obj)
  (let* ((objptr (object->pointer obj))
         (ptr (pointer->integer objptr))
         (ptrbox (make-weak-box))
         (box (make-weak-box)))
    (weak-box-set! ptrbox ptr)
    (register_disappearing_link (bytevector-pointer box) objptr)
    (cons ptrbox box)))

(define (guardian-query l)
  (define (itr prev cur)
    (if (pair? cur)
      (let ((a (car cur))
            (d (cdr cur)))
        (if (weak-box-broken? (cdr a))
          (let ((obj (pointer->object (weak-box-ref (car a)))))
            (display (list 'return: obj))(newline)
            (cond
              ((null? prev)
               (values d obj))
              (else
                (set-cdr! prev d)
                (values l obj))))
          (itr cur d)))
      (values l #f)))
  (itr '() l))

(define (make-guardian)
  (define citizen '())
  (case-lambda
    (()
     (receive (new-citizen ret) (guardian-query citizen)
       (set! citizen new-citizen)
       ret))
    ((x)
     (set! citizen
       (cons (hide x)
             citizen)))))

)
