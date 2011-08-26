(library (nmosh pffi posix util)
         (export
           construct-string-ptrs)
         (import (rnrs)
                 (shorten)
                 (srfi :42)
                 (nmosh pffi interface))

(define (construct-string-ptrs l)
  (let* ((c (length l))
         (setter (if (= 4 size-of-pointer)
                   bytevector-u32-native-set!
                   bytevector-u64-native-set!))
         (bv (make-bytevector (* size-of-pointer (+ c 1))))
         (r (list->vector (map (^e (bytevector-pointer (string->utf8 e))) l))))
    (do-ec (: i c)
           (setter bv
                   (* size-of-pointer i)
                   (pointer->integer (vector-ref r i))))
    ;; add NULL
    (setter bv (* size-of-pointer c) 0)
    bv))

)
