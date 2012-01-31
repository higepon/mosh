(library (nmosh ffi pffi-plugin)
         (export plugin-path
                 make-pffi-ref/plugin
                 pffi-c-function)
         (import (rnrs)
                 (mosh)
                 (mosh ffi)
                 (nmosh ffi pffi)
                 (nmosh ffi pffi-plugin platform)
                 (nmosh global-flags)
                 (yuni util files))


(define plugin-initialized? #f)

(define (make-pffi-ref/plugin name)
  (unless plugin-initialized?
    (plugin-init (plugin-path))
    (set! plugin-initialized? #t))
  (let* ((stdpath (plugin-path))
         (plugin-file (path-append stdpath (string-append
                                             (symbol->string name)
                                             ".mplg"))))
    (open-shared-library plugin-file)))

(define (plugin-path)
  (let ((f (get-global-flag '%nmosh-prefixless-mode)))
    (if f
      (let ((basepath (path-dirname (mosh-executable-path))))
        (path-append basepath "plugins"))
      (path-append (standard-library-path) "plugins"))))

)
