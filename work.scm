(import (rnrs)
        (mosh ffi)
        (mosh)
        (mosh test))


  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))

    (define return_struct (c-function libffitest void* return_struct))
    (define struct_ref (c-function libffitest char struct_ref void*))

   (let ([p (return_struct)])
     (test-eq -1 (struct_ref p))))

(test-results)
