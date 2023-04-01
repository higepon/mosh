(library (nmosh bootstrap compiler)
         (export compile-to-sexp
                 compile-to-sexp-w/o-halt)
         (import (rnrs)
                 (nmosh bootstrap stubs)
                 (nmosh boot src-config)
                 (only (nmosh boot compiler) compile compile-w/o-halt))

(define (compile-to-sexp code)
  (encode-compiler-output (compile code)))

(define (compile-to-sexp-w/o-halt code)
  (encode-compiler-output (compile-w/o-halt code)))

(init-compiler src-free-vars instructions)
)
