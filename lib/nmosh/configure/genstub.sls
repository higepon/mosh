(library (nmosh configure genstub)
         (export genstub)
         (import (rnrs)
                 (yuni util tables scheme)
                 (yuni core)
                 (yuni util files)
                 (srfi :48)
                 (mosh pp)
                 (shorten))


(define* constant (name value type))

(define (proc pth filename table*)
  (define internal? #f)
  (define myname #f)
  (define plugin-name #f)
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
        ((int char* int* void* void double) ;; verbatim output
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
        (pp `(define ,name (pffi-c-function 
                             %library
                             ,(convtype ret) 
                             ,name ,@(if args (map convtype args) '()))) p))
      (table-for-each tbl '(ret name args) emit))
    ;; emit header
    (format p ";; generated from ~a DO NOT EDIT!!\n" pth)
    (format p "(library (nmosh stubs ~a)\n" myname)
    (pp `(export ,@exports) p)
    (pp `(import (mosh ffi) (rnrs) 
                 ,(if plugin-name
                    '(nmosh ffi pffi-plugin)
                    '(nmosh ffi pffi))
                 (nmosh ffi stublib)) p)

    ;; emit globals (handle for shared-library or pffi)
    (cond
      (plugin-name
        (format p "\n\n(define %library (make-pffi-ref/plugin '~a))\n" 
                plugin-name))
      (internal?
        (format p "\n\n(define %library (make-pffi-ref '~a))\n" libname))
      (else
        (format p "\n\n(define-ffi-library %library ~a ~a)\n" libname libname)))

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

  ;; collect internal?
  (for-each-tablesym '*internal* (^ _ (set! internal? #t)))

  ;; collect plugin name
  (for-each (^e (let ((name (table-metadata-ref e 'plugin:)))
                  (when name (set! plugin-name name))))
            table*)

  ;; collect myname
  (for-each (^e (let ((name (table-metadata-ref e 'libname:)))
                  (when name (set! myname name))))
            table*)

  (unless myname
    (assertion-violation #f "Please specify library name"
                         pth))


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
  (call-with-output-file filename output)
  (display filename))

(define (genstub pth outpath)
  (format #t "generating library for ~a\n" pth)
  (proc pth outpath (file->table-list pth))
  (display " generated.\n"))

)
