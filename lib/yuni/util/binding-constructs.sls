(library (yuni util binding-constructs)
         (export 
           let-optionals*
           let1
           define-values)
         (import (rnrs))

(define-syntax cdr/nil
  (syntax-rules ()
    ((_ x)
     (if (pair? x) (cdr x) '()))))

(define-syntax leto*1
  (syntax-rules ()
    ((_ args (var0 default0) body ...)
     (let1 var0 (if (pair? args) (car args) default0)
           body ...))))

(define-syntax leto*
  (syntax-rules ()
    ((_ args ((var default) restarg) body ...)
     (leto*1 args (var default)
             (let1 restarg (cdr/nil args) body ...)))
    ((_ args ((var0 default0) (var1 default1) ... restarg) body ...)
     (leto*1 args (var0 default0)
             (leto* (cdr/nil args) ((var1 default1) ... restarg) body ...)))))



(define-syntax let-optionals*
  (syntax-rules ()
    ((_ args ((var default) ...) body ...)
     (let-optionals* args ((var default) ... bogus) body ...))
    ((_ args ((var default) ... restarg) body ...)
     (leto* args ((var default) ... restarg) body ...))))

(define-syntax let1
  (syntax-rules ()
    ((_ obj tm body ...)
     (let ((obj tm))
       body ...))))

(define-syntax define-values
  (lambda (x)
    (syntax-case x ()
      ((_ (val ...) body)
       (with-syntax (((name ...)
                      (generate-temporaries #'(val ...)))
                     ((tmp ...)
                      (generate-temporaries #'(val ...))))
         #'(begin
             (define name #f)
             ...
             (define bogus
               (begin
                 (call-with-values (lambda () body)
                                   (lambda (tmp ...)
                                     (set! name tmp)
                                     ...
                                     ))))
             (define val name)
             ...
             ))))))

)

