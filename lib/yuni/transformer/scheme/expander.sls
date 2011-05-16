(library (yuni transformer scheme expander)
         (export make-library-env
                 make-default-library-env
                 ;import-default-libraries
                 make-stage)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs files)
                 (rnrs control)
                 (rnrs eval)
                 (srfi :8)
                 (nmosh gensym)
                 (only (mosh) library-path)
                 (shorten)
                 (yuni core)
                 (except (yuni util library-files) 
                         library-name 
                         make-library-env)
                 (yuni util files)
                 (yuni transformer scheme syntax-rules-transformer)
                 (yuni transformer scheme identifier-object)
                 (yuni transformer scheme annotated-reader))

(define* (symeq? (id identifier) sym)
  (eq? (syntax->datum id) sym))

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

(define* (resolve-alias (env library-env) spec)
  (let-with env (alias*)
    (define (itr cur x)
      (if (pair? cur)
        (let ((name (car cur))
              (rest (cdr cur)))
          (if (equal? name x)
            (itr alias* name)
            (itr rest x)))
        x))
    (itr alias* spec)))

(define* (locate-library (env library-env) spec) ;; => string
  (let* ((l (map symbol->path (resolve-alias env (omit-versions spec))))
         (basepath (fold-left path-append "" l)))
    (let-with env (loadpath*)
      (find (^[dir]
              (find (^[ext]
                      (let ((name (path-append dir 
                                               (string-append basepath ext))))
                        (and (file-exists? name) name)))
                    '(".sls" ".sps" ".ss" ".sch" ".scm")))
            loadpath*))))

(define* (stage-library (stage library-stage) (lib library))
  ;; FIXME: check overload
  (let-with stage ((current-loaded loaded-library*))
    (touch! stage
            (loaded-library* (cons lib current-loaded)))))

(define* (stage-library-syntax (stage library-stage) filename stx)
  (let ((id (syntax->datum (car stx))))
    (if (eq? 'library id)
      (let ((libname (syntax->datum (cadr stx)))
            (export-spec (syntax->datum (caddr stx)))
            (program (expand-program stage filename (cdddr stx))))
        ;; evaluate library code ...
        ;; FIXME: process export-spec
        (stage-library stage
                       (make library
                             (name libname)
                             (type 'expanded)
                             (source filename)
                             (code program)
                             (export* '()))))
      (assertion-violation 'stage-library-syntax
                           "invalid program format"
                           filename))))

(define* (stage-library-file (stage library-stage) filename)
  (let ((source (read-source filename)))
    (for-each (^[lib] (stage-library-syntax stage filename lib))
              source)))

(define core-name '(*core-library*))

(define* (stage-core-syntax (stage library-stage))
  (stage-library stage
                 (make library
                       (name core-name)
                       (type 'core)
                       (source #f)
                       (code #f)
                       (export* core-syntax))))

(define (read-source filename)
  (define (inject sym current-position)
    (make-identifier sym #f (list "read-source" current-position)))
  (file->source filename inject))

(define (make-core-syntax sym)
  (make binding
        (name sym)
        (global-name sym)
        (type 'core-macro)
        (id (make-identifier sym core-name (list "core-library" sym)))))

(define* library-stage (library-env loaded-library*))

(define* (make-stage (env library-env))
  (make library-stage
        (library-env env)
        (loaded-library* '())))

(define* library (name type source code export*))
(define* (library-name (lib library))
  (let-with lib (name) name))

;; binding
;;   type := core-macro | macro | variable ;;;; identifier-macro..?
(define* binding (name global-name id type code proc))

(define* (binding-id (b binding)) (let-with b (id) id))
(define* (binding-name (b binding)) (let-with b (name) name))
(define* (binding-global-name (b binding)) (let-with b (global-name) global-name))

(define* (rename-binding (old binding) sym)
  (let-with old (global-name id type)
    (make binding
          (name sym)
          (global-name global-name)
          (id id)
          (type type))))

(define (make-primitive-binding sym)
  (make binding
        (name sym)
        (global-name sym)
        (id (make-identifier sym #f (list "primitive" sym)))
        (code #f)
        (proc #f)
        (type 'variable)))

(define (make-primitive-bindings sym*)
  (map make-primitive-binding sym*))

(define* (core-syntax? (binding))
  (let-with binding (type)
    (eq? type 'core-macro)))
(define* (macro? (binding))
  (let-with binding (type)
    (eq? type 'macro)))
(define* (variable? (binding))
  (let-with binding (type)
    (eq? type 'variable)))

(define-syntax define-core-syntax*
  (syntax-rules ()
    ((_ (def name))
     (define def (make-core-syntax 'name)))))

(define-syntax core-syntax*
  (syntax-rules ()
    ((_ name (name0 value0 ...) ...)
     (begin
       (define-core-syntax* (name0 value0 ...))
       ...
       (define name (list name0 ...))))))

(core-syntax* core-syntax
              (core:define-syntax define-syntax)
              (core:let-syntax let-syntax)
              (core:letrec-syntax letrec-syntax)
              (core:quote core-quote)
              (core:form core-form)
              (core:extend core-extend)
              (core:extend-define core-extend-define)
              (core:invalid-form core-invalid-form)
              (core:expand-form expand-form)
              (core:expand-body expand-body)
              (core:expand-begin expand-begin))

(define (env-lookup env sym) ;; => binding / #f
  (define (do-lookup bind*)
    (find (^b (let-with b (name) (and (eq? name sym) b))) bind*))
  (find do-lookup env))

(define (fix-id env id)
  (let-with id (name library)
    (if library ;; bound?
      id
      (let ((b (env-lookup env name)))
        (if b
          (let-with b ((libid id)) (identifier-rename/id id libid "env"))
          id)))))

(define (expand-sequence stage exp env) ;; => exp de
  (define* (invoke-transformer (binding) exp) ;; => exp de
    (let-with binding (id)
      (let-with id (library)
        (define rename (get-library-renamer stage library))
        (define (normalize id) (if (symbol? id) (rename id) id))
        (define (compare id0 id1)
          (let ((id0 (fix-id env (normalize id0)))
                (id1 (fix-id env (normalize id1))))
            (free-identifier=? id0 id1)))
        (define inject
          (case-lambda
            (() (make-identifier (gensym) #f "gensym"))
            ((sym) 
             (let ((binding? (env-lookup env sym)))
               (or (and binding? (binding-id binding?))
                   (make-identifier sym #f "inject"))))))
        (define transformer (transformer-lookup stage binding))
        (expand-sequence stage
                         (transformer exp rename compare inject)
                         env))))
  (define (rename-variable x)
    (if (pair? x)
      (cons (receive (exp _) (expand-sequence stage (car x) env) exp)
            (receive (exp _) (expand-sequence stage (cdr x) env) exp))
      (if (identifier? x)
        (let ((r (env-lookup env (identifier-name x))))
          (if r
            (let-with r (global-name) global-name)
            (assertion-violation 'expand-sequence
                                 "unbound variable"
                                 x)))
        x)))
  (if (pair? exp)
    (let ((head (car exp))
          (rest (cdr exp)))
      (if (identifier? head)
        (let ((headv (env-lookup env (identifier-name head))))
          (if headv
            (cond
              ((macro? headv) ;; invoke transformer
               (invoke-transformer headv exp))
              ((core-syntax? headv)
               )
              (else ;; application
                (values (map rename-variable exp) env)))))
        (assertion-violation 'expand-sequence
                             "invalid form"
                             exp)))
    (values (rename-variable exp) env)))

(define (expand-sequence/toplevel stage exp env) ;; => exp de
  )

(define* (stage-import (stage library-stage) filename spec) ;; => binding*
  (let-with stage (loaded-library*)
    (let ((lib (find (^e (equal? (library-name e) spec)) loaded-library*)))
      (if lib
        (let-with lib (export*) export*)
        (let-with stage (env)
          (stage-library-file stage (locate-library env spec))
          ;; FIXME: check if really imported here..
          (stage-import stage filename spec))))))

(define* (expand-program (stage library-stage) filename stx)
  (define (make-binding clause k)
    ;; dispatch
    (let ((op (car clause)))
      (cond
        ((symeq? op 'for)
         ;; FIXME: ignore "for"
         (make-binding (cadr clause) k))
        ((symeq? op 'except)
         ;; FIXME: ignore "except"
         (make-binding (cadr clause) k))
        ((symeq? op 'primitives)
         (k (make-primitive-bindings (syntax->datum (cdr clause)))))
        ((symeq? op 'rename)
         (let ((rename-ops (syntax->datum (caddr clause))))
           (define (search-name sym)
             (or (find (^[clause]
                         (let ((from (car clause))
                               (to (cadr clause)))
                           (and (eq? from sym) to)))
                       rename-ops)
                 ;; FIXME: claim something here...
                 #f))
           (define (do-rename binding*)
             (define* (renamer (old-binding binding))
               (let-with old-binding (name)
                 (let ((newname (search-name name)))
                   (if newname
                     (rename-binding old-binding newname)
                     old-binding))))
             (map renamer binding*))
           (make-binding (cadr clause)
                         (^[b*] (k (do-rename b*))))))
        (else
          (k (stage-import stage filename clause))))))
  (define (do-import clause) 
    (make-binding clause (^[binding*] binding*)))
  (define (process-import stx)
    (let ((import? (car stx))
          (import-clause* (cdr stx)))
      (unless (symeq? import? 'import)
        (assertion-violation 'process-import
                             "malformed import form"
                             (syntax->datum stx)))
      (fold-left append '() (map do-import import-clause*))))
  (let ((top-binding (process-import (car stx))))
    (expand-sequence/toplevel stage (cdr stx) (list top-binding))))
)
