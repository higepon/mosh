(import (rnrs)
        (mosh pp)
        (yuni util files)
        (shorten))

(define (listup pth)
  (define cur '())
  (directory-walk pth (^e (set! cur (cons e cur))))
  cur)

(define (library? str)
  (or (string=? str "ss")
      (string=? str "sls")
      (string=? str "scm")
      (string=? str "sch")
      (string=? str "sps")))

(define (output l)
  (when (file-exists? "automake/corelibs.mk")
    (delete-file "automake/corelibs.mk"))
  (call-with-output-file
    "automake/corelibs.mk"
    (^p (display "mosh_core_libraries = " p)
        (for-each (^e (display " \\\n" p) (display e p) ) l)
        (display "\\\n$(top_builddir)/lib/mosh/config.ss" p)
        (display "\\\n$(top_builddir)/lib/mosh/mysql.ss" p)
        (display "\n\n" p))))

(output (filter (^e (let ((p (path-extension e)))
                      (and p (library? p)))) (listup "lib")))

