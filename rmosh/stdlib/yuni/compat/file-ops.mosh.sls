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
         (import (rnrs)
                 (mosh)
                 (mosh file))
(define (system-msdos-style-path?)
  (or (string=? "win32" (host-os))
      (string=? "win64" (host-os))))

)


