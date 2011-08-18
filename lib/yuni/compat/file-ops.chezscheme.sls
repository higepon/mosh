(library (yuni compat file-ops)
         (export 
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; mosh directory procedure
           create-directory
           delete-directory

           ;; ancient
           system-msdos-style-path?
           )
         (import (chezscheme))
;; FIXME: stub
(define (system-msdos-style-path?)
  #f)
(define create-directory mkdir)

)


