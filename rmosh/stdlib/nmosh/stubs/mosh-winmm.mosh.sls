;; generated from src/win32/plugins/mm/Library.scm DO NOT EDIT!!
(library (nmosh stubs mosh-winmm)
(export mmm_joy_read mmm_joy_count)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi-plugin)
  (nmosh ffi stublib))


(define %library (make-pffi-ref/plugin 'mosh_winmm))


(define
  mmm_joy_count
  (pffi-c-function %library int mmm_joy_count))
(define
  mmm_joy_read
  (pffi-c-function
    %library
    int
    mmm_joy_read
    int
    void*))
)
