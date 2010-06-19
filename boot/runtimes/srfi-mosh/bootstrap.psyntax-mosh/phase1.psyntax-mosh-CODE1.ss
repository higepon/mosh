(when (file-exists? "BOOT1.exp") (delete-file "BOOT1.exp"))
(display "expanding...")
(let* ((core (ex:expand-sequence core-src))
       (expander (ex:expand-sequence-r5rs expander-src (ex:environment '(rnrs base))))
       (code (append core expander)))
  (display "writing BOOT1.exp")(newline)
  (call-with-output-file "BOOT1.exp"
                         (lambda (p)
                           (for-each
                             (lambda (e)
                               (write e p)
                               (newline p))
                             code))))
