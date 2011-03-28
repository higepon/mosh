(library (yuni transformer scheme expander)
         (export make-library-env
                 make-default-library-env
                 ;import-default-libraries
                 make-stage)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs files)
                 (only (mosh) library-path)
                 (shorten)
                 (yuni core)
                 (except (yuni util library-files) make-library-env)
                 (yuni util files)
                 (yuni transformer scheme syntax-rules-transformer)
                 (yuni transformer scheme identifier-object)
                 (yuni transformer scheme annotated-reader))

(define* library-env
  (loadpath*))

(define (make-library-env)
  (make library-env
        (loadpath* '())
        (alias* '())))

(define (make-default-library-env)
  (make library-env
        (loadpath* (library-path))
        (alias* '())))

(define (valid-libspec-component? x)
  (or (symbol? x)
      ;; followings are WG1 extension
      (number? x)
      (string? x)))

(define (omit-versions spec)
  (if (pair? spec)
    (let ((a (car spec))
          (d (cdr spec)))
      (if (valid-libspec-component? a)
        (cons a (omit-versions d))
        '()))
    '()))

(define* (locate-library (env library-env) spec) ;; => string
  (let* ((l (map symbol->path (omit-versions spec)))
         (basepath (fold-left path-append "" l)))
    (let-with env (loadpath*)
      (find (^[dir]
              (find (^[ext]
                      (file-exists?
                        (path-append dir (string-append basepath ext))))
                    '(".sls" ".sps" ".ss" ".sch" ".scm")))
            loadpath*))))

(define* library-stage (library-env loaded-library*))

(define* (make-stage (env library-env))
  (make library-stage
        (library-env env)
        (loaded-library* '())))

(define* library (name type source code export*))

(define* library-export (name type obj env))

;; core expanders:
;;
;; define-syntax
;; let-syntax
;; letrec-syntax
;;
;; core-define
;; core-quote
;; core-form
;; core-extend
;; core-invalid-form
;; expand-form
;; expand-body
;; expand-name


)
