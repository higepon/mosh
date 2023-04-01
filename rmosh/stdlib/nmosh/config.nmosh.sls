(library (nmosh config)
         (export config-path
                 config-load
                 config-reload)
         (import (rnrs)
                 (mosh)
                 (shorten)
                 (yuni util files)
                 (srfi :98)
                 (only (mosh pp) pp)
                 (nmosh global-flags))

(define config-data #f)
(define config-path-data #f)

(define (config-value) config-data)

(define (config-reload)
  (set! config-data #f)
  (config-load))

(define (config-load) ;; DATA/FALSE
  (unless config-data
    (let ((path (config-path)))
      (when path (set! config-data (file->sexp-list (config-path))))))
  config-data)

(define (config-path)
  (if config-path-data
    config-path-data
    (begin (set-config-path!)
           config-path-data)))

(define (get-home-path)
  (let ((os (host-os)))
    (cond
      ((string=? os "win32")
       (or (get-environment-variable "LOCALAPPDATA")
           (get-environment-variable "APPDATA")))
      (else (get-environment-variable "HOME")))))

(define (set-config-path!)
  (define (embedded-mode?) (get-global-flag '%nmosh-portable-mode))
  (set! config-path-data
    (cond
      ((embedded-mode?)
       (path-append (mosh-executable-path) "config"))
      (else ;;prefix-less or normal mode
        (let ((home (get-home-path)))
          (and home 
               (path-append home ".nmosh")))))))

)

