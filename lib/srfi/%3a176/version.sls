(library (srfi :176 version)
         (export version-alist)
         (import (rnrs)
                 (only (mosh config) get-config)
                 (only (mosh) library-path mosh-executable-name))

(define (version-alist)
  `((version ,(get-config "version"))
      (command ,(mosh-executable-name))
      (scheme.id mosh)
      (install-dir ,(get-config "prefix"))
      (languages scheme r6rs)
      (encodings utf-8)
      (mosh.cache-dir ,(get-config "mosh-cache-dir"))
      (website "https://mosh.monaos.org/")
      (scheme.path ,@(library-path))
  ))
)
