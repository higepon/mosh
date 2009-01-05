(import (rnrs)
        (mosh)
        (system))


;; (define-syntax define-structure
;;   (lambda (x)
;;      (define construct-name
;;         (lambda (template-identifier . args)
;;            (implicit-identifier
;;               template-identifier
;;               (string->symbol
;;                  (apply string-append
;;                         (map (lambda (x)
;;                                 (if (string? x)
;;                                     x
;;                                     (symbol->string (syntax-object->datum x))))
;;                              args))))))
;;      (syntax-case x ()
;;         ((_ (name id1 ...))
;;          (syntax (define-structure (name id1 ...) ())))
;;         ((_ (name id1 ...) ((id2 init) ...))
;;          (with-syntax
;;             ((constructor (construct-name (syntax name) "make-" (syntax name)))
;;              (predicate (construct-name (syntax name) (syntax name) "?"))
;;              ((access ...)

;; (define-syntax hoge
;;   (lambda (x)
;;     (syntax-case x ()
;;       [(_ name)
;;        (let ([x (datum->syntax #'name (string->symbol (string-append (symbol->string 'name) "$")))])
;;         (define x 3)
;;         #f)])))
;; (hoge hige)
;; (display hige$)

(define-syntax $define
  (lambda (x)
    (syntax-case x ()
      ((k var val)
       (with-syntax ([var (datum->syntax #'k (string->symbol
                           (format "$~a" (syntax->datum #'var))))])
         #'(define var val))))))

($define a 3)
(display $a)



(define-syntax my-define
  (lambda(x)
    (syntax-case x ()
      ((k v s)
       (with-syntax ((nv
                      (datum->syntax (syntax k)
                                     (string->symbol
                                      (string-append
                                       "$"
                                       (symbol->string (syntax->datum (syntax v))))))))
                    (syntax (define nv s)))))))



(define-syntax my-define
  (lambda(x)
    (syntax-case x ()
      ((k v s)
       (with-syntax ((nv (string->symbol
                                      (string-append
                                       "$"
                                       (symbol->string (syntax->datum (syntax v)))))))
                    (syntax (define nv s)))))))
