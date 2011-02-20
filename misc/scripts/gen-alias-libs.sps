(import (yuni util library-files)
        (yuni util tables scheme)
        (yuni util files)
        (yuni core)
        (only (srfi :1) delete-duplicates!)
        (srfi :8)
        (srfi :48)
        (mosh pp)
        (rnrs)
        (shorten))

(define generated-file '())
(define (add-generated-file! x)
  (set! generated-file (cons x generated-file)))
(define generated-library '())
(define (add-generated-library! x)
  (unless (find (^p (equal? x p)) generated-library)
    (set! generated-library (cons x generated-library))))

(define* entry (name baselib* level))

(define tbl (file->table-list "lib/config/library-config.scm"))

(define (for-each-sym sym proc)
  (for-each (^e (when (table-metadata-ref e sym)
                  (proc e)))
            tbl))

(define entry* '())
(define* (add-entry! (x entry))
  (set! entry* (cons x entry*)))

(define (locate-library- pth)
  (define (check suf)
    (let ((name (string-append pth "." suf)))
      (if (file-exists? name)
        name #f)))
  (or
    (check "sls")
    (check "sps")
    (check "ss")
    (check "sch")
    (check "scm")))

(define (locate-library pth)
  (or
    (locate-library- (string-append pth ".mosh"))
    (locate-library- (string-append pth ".nmosh"))
    (locate-library- pth)))


(define (library-basepath x)
  (fold-left (^[cur e] (path-append cur e))
             "lib"
             (map symbol->path x)))

(define (library-path x)
  (locate-library (library-basepath x)))

(define (library-export x)
  (define (gen-export l)
    (let ((exports (caddr l)))
      (fold-left (^[cur e]
                   (if (list? e)
                     (append cur (map (^l (cadr l)) (cdr e)))
                     (cons e cur)))
                 '()
                 (cdr exports))))
  (define (proc lib)
    (if (and (pair? lib)
             (pair? (car lib))
             (eq? 'library (caar lib))
             (equal? x (cadar lib)))
      (gen-export (car lib))
      (if (pair? lib) (proc (cdr lib)))))
  (let ((path (library-path x)))
    (if path
      (proc (file->sexp-list path))
      #f)))

(define (emit-aliaslib libname base*)
  (define syms0 (apply append (map library-export base*)))
  ;(pp (list 'SYMS0 syms0))
  (let ((syms (if syms0 (delete-duplicates! syms0) '()))
        (output (string-append (library-basepath libname)
                               ".mosh.sls"))
        (origpath (map library-path base*)))
    ;(pp (list 'SYMS syms))
    (when (file-exists? output)
      (delete-file output))
    (when (for-all (^e e) origpath)
      (format #t "generate: ~a <= ~a (~a exports)\n" libname 
              base* 
              (length syms))
      (call-with-output-file
        output
        (^p (format p ";; this file is an alias-library.\n")
            (format p ";;  alias of:\n")
            (for-each (^e (format p ";;   ~a\n" e))
                      origpath)
            (format p "(library ~a\n" libname)
            (format p "         (export\n")
            (for-each (^e (format p "             ~a\n" e))
                      syms)
            (format p "         )\n")
            (format p "         (import\n")
            (for-each (^e (format p "             ~a\n" e))
                      base*)
            (format p "         )\n")
            (format p ") ;; library ~a\n" libname))) 
      (for-each (^x (add-generated-library! x)) base*)
      (add-generated-library! libname)
      (add-generated-file! output))))

(define* (generate-alias (e entry))
  (let-with e (name baselib* level)
    ;; ignore rnrs libraries here.
    ;; rnrs composite libraries exports some symbol in
    ;; different export level.
    (when (and (list? baselib*) (not (eq? level 'rnrs)))
      (emit-aliaslib name baselib*))))

;; filter out multi-level alias
(define* (has-multi-level? (e entry))
  (define (aliased? x)
    (find (^e (let-with e (name) (equal? x name))) entry*))
  (let-with e (name baselib*)
    (not 
      (and (list? baselib*)
           (for-all
             (^p (not (aliased? p)))
             baselib*)))))

;; collect all library definition

(for-each
  (^e (table-for-each 
        e '(alias-name library-name interface-level)
        (^[n b l] (add-entry! (make entry
                                    (name n)
                                    (baselib* (if (and b (list? (car b)))
                                                b
                                                (if b (list b) #f)))
                                    (level l))))))
  tbl)

;; generate alias library
(receive (multi-levels first) (partition has-multi-level? entry*)
  (for-each generate-alias first)
  (for-each generate-alias multi-levels))

#|
;; update .gitignore
(let ()
  (define gitignore (file->string-list ".gitignore"))
  (define (output p cur)
    (define (line str)
      (display str p)
      (newline p))
    (define (skip cur)
      (if (string=? (car cur)
                    "# -- GENERATED ALIAS LIBRARIES --")
        cur
        (skip (cdr cur))))
    (define (output-generated)
      (for-each (^l (line l)) generated))
    (if (pair? cur)
      (let ((a (car cur))
            (rest (cdr cur)))
        (line a)
        (cond 
          ((string=? a "# -- GENERATED ALIAS LIBRARIES --")
           (output-generated)
           (line "# -- GENERATED ALIAS LIBRARIES --")
           (output p (skip rest)))
          (else (output p rest))))
      'ok))
  (delete-file ".gitignore")
  (call-with-output-file
    ".gitignore"
    (^p (output p gitignore))))
|#
