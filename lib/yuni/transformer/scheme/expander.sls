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

;; binding
;;   type := macro | variable ;;;; identifier-macro..?
(define* binding (name global-name id type))

(define* (macro? (binding))
  (let-with binding (type)
    (eq? type 'macro)))
(define* (variable? (binding))
  (let-with binding (type)
    (eq? type 'variable)))

(define-syntax define-core-syntax*
  (syntax-rules ()
    ((_ (def name))
     (define def (make-core-syntax 'name)))
    ((_ cl0 cl1 ...)
     (begin
       (define-core-syntax* cl0)
       (define-core-syntax* cl1 ...)))))

(define-syntax core-syntax*
  (syntax-rules ()
    ((_ name ((name0 value0 ...) ...))
     (begin
       (define-core-syntax* (name0 value0 ...) ...)
       (define name (list name0 ...))))))

(core-syntax* core-syntax
              (core:define-syntax 'define-syntax)
              (core:let-syntax 'let-syntax)
              (core:letrec-syntax 'letrec-syntax)
              (core:quote 'core-quote)
              (core:form 'core-form)
              (core:extend 'core-extend)
              (core:extend-define 'core-extend-define)
              (core:invalid-form 'core-invalid-form)
              (core:expand-form 'expand-form)
              (core:expand-body 'expand-body)
              (core:expand-begin 'expand-begin))

;; core expanders:
;;
;; define-syntax
;; let-syntax
;; letrec-syntax
;;
;; core-quote
;; core-form
;; core-extend
;; core-extend-define
;; core-invalid-form
;; expand-form
;; expand-body
;; expand-begin

(define (expand-sequence exp de le) ;; => exp de

  )

)
