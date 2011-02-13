(import (yuni util tables scheme)
        (rnrs)
        (yuni core)
        (yuni util files)
        (srfi :48)
        (mosh pp)
        (shorten))

(define targets '("src/ext" "src/posix" "src/win32"))

(define (locate-Library)
  (define libs '())
  (define (add-lib! lib)
    (set! libs (cons lib libs)))
  (for-each (^e (directory-walk 
                  e 
                  (^p (when (string=? (path-basename p) "Library.scm")
                        (add-lib! p)))))
            targets)
  libs)

(define libs (locate-Library))

(define* constant (name value type))

(define (proc pth table*)
  (define filename)
  (define myname #f)
  (define libname)
  (define constants '())
  (define exports '())
  (define objects '())
  (define (add-constant! name value type)
    (add-export! name)
    (set! constants
      (cons (make constant
                  (name name)
                  (value value)
                  (type (if type type 'int)))
            constants)))
  (define (add-export! sym)
    (set! exports (cons sym exports)))
  (define (add-object! sym)
    (set! objects (cons sym objects)))
  (define (for-each-tablesym sym proc) 
    (for-each (^e 
                (when (table-metadata-ref e sym)
                  (proc e))
                proc table*)
              table*))
  (define (collect-constants tbl)
    (table-for-each tbl '(name value type) add-constant!))

  (define (convtype sym)
    (define (pointer? str)
      (char=? #\* (list-ref (reverse (string->list str)) 0)))
    (let ((str (symbol->string sym)))
      (case sym 
        ((int char* int* void* void) ;; verbatim output
         sym)
        ((fn) 
         ;; callback is not always a callback..
         'callback)
        (else
          (cond
            ((pointer? str)
             'void*)
            (else
              (assertion-violation #f "invalid type" sym)))))))

  (define (output p)
    (define (emit-function tbl)
      (define (emit ret name args)
        ;; emit function definition
        (pp `(define ,name (c-function 
                             %library
                             ,(convtype ret) 
                             ,name ,@(if args (map convtype args) '()))) p))
      (table-for-each tbl '(ret name args) emit))
    ;; emit header
    (format p ";; generated from ~a DO NOT EDIT!!\n" pth)
    (format p "(library (nmosh stubs ~a)\n" myname)
    (pp `(export ,@exports) p)
    (pp '(import (mosh ffi) (rnrs) (nmosh ffi stublib)) p)

    ;; emit globals (handle for shared-library)
    (format p "\n\n(define-ffi-library %library ~a ~a)\n" libname libname)

    (newline p)

    ;; emit constants
    (let ((bodies (map (^e (let-with e (name value type)
                             (if (eq? type 'void*)
                               `(define ,name (integer->pointer ,value))
                               `(define ,name ,value))))
                       constants)))
      (for-each (^e (pp e p)) bodies))

    (newline p)

    ;; collect and emit functions
    (for-each-tablesym 'c-function-table emit-function)
    ;; emit footer
    (display ")\n" p))

  ;; collect myname
  (for-each (^e (let ((name (table-metadata-ref e 'libname:)))
                  (when name (set! myname name))))
            table*)

  (unless myname
    (assertion-violation #f "Please specify library name"
                         pth))

  (set! filename (format "lib/nmosh/stubs/~a.mosh.sls" myname))

  ;; collect libname
  (for-each (^e (let ((name (table-metadata-ref e 'libname:)))
                  (when name (set! libname name))))
            table*)

  (unless libname
    (assertion-violation #f "Please specify soname"
                         pth))

  ;; collect constants
  (for-each-tablesym 'constant-table collect-constants)
  ;; collect objects (opaque structures or typedefs)
  ;; collect functions (pass1: exports)
  (for-each-tablesym 'c-function-table 
                     (^t (table-for-each 
                           t
                           '(name)
                           (^[name]
                             (add-export! name)))))

  ;; output
  (when (file-exists? filename)
    (delete-file filename))
  (call-with-output-file filename output))

(define (gen-lib pth)
  (format #t "generating library for ~a\n" pth)
  (proc pth (file->table-list pth))
  (display "done.\n"))

(for-each gen-lib libs)

