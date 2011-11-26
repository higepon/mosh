;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/file.sls
(library (scheme file)
         (export
             with-output-to-file
             with-input-from-file
             open-output-file
             open-input-file
             open-binary-output-file
             open-binary-input-file
             file-exists?
             delete-file
             call-with-output-file
             call-with-input-file
         )
         (import
             (r7b-impl file)
         )
) ;; library (scheme file)
