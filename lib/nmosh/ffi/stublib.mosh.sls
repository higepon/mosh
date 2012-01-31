(library (nmosh ffi stublib)
         (export define-ffi-library)
         (import (rnrs)
                 (mosh ffi))

(define soext 
  (cond
    (on-darwin ".dylib")
    (else ".so")))

(define (soname basename)
  (string-append "lib" basename soext))

(define (search-and-open-shared-libary spec libname)
  (let ((name (soname (symbol->string spec))))
    (let ((so (open-shared-library name)))
      so)))

(define-syntax define-ffi-library
  (syntax-rules ()
    ((_ name spec libname)
     (define name (search-and-open-shared-libary 'spec 'libname)))))

)
