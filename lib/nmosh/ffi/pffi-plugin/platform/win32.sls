(library (nmosh ffi pffi-plugin platform win32)
         (export plugin-init)
         (import (rnrs)
                 (srfi :98)
                 (nmosh pffi win32 env))

;;
(define (plugin-init plugin-path)
  ;; Add plugin path to PATH
  (setenv "PATH"
          (string-append plugin-path ";"
                         (get-environment-variable "PATH")))) 

)
