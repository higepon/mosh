(library (nmosh ffi pffi-plugin platform win32)
         (export plugin-init)
         (import (rnrs)
                 (srfi :98)
                 (nmosh pffi win32 env))

;;
;; FIXME:
(define (try-get-path)
  (let ((e (map (lambda (e)
                  (cons (string-upcase (car e))
                        (cdr e)))
                (get-environment-variables))))
    (let ((p (assoc "PATH" e)))
      (cdr p))))

(define (plugin-init plugin-path)
  ;; Add plugin path to PATH
  (setenv "PATH"
          (string-append plugin-path ";"
                         (try-get-path)))) 

)
