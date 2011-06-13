(library (yuni text config writer)
         (export config->file)
         (import (rnrs)
                 (yuni util files)
                 (yuni text config reader))

(define (section->string-list l)
  (define (makeheader x)
    (if (= (length x) 1)
      (string-append "[" (car x) "]")
      (string-append "[" (car x) " " "\"" (cadr x) "\"]")))
  (define (makeentry x)
    ;; FIXME: escape it??
    (string-append "\t" (car x) " = " (cdr x)))
  (let ((header (makeheader (car l)))
        (entries (map makeentry (cdr l))))
    (cons header (append entries (list "")))))

(define (config-generate pth l)
  (define (clean-up l)
    (reverse (let loop ((cur (reverse l)))
               (or (and (pair? cur)
                        (string=? "" (car cur))
                        (loop (cdr cur)))
                   cur))))
  (string-list->file pth
                     (clean-up (fold-left append 
                                          '() (map section->string-list l)))))

(define (config->file pth l)
  ;; FIXME: always overwrite for now
  (config-generate pth l))


)
