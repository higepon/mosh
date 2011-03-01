;; MIT/GNU scheme compatible er-macro-transformer in R6RS syncase
(library (yuni scheme explicit-renaming)
         (export er-macro-transformer 
                 er-macro-transformer+
                 identifier?)
         (import (rnrs)
                 (yuni compat gensym)
                 (yuni scheme unwrap-syntax))

(define-syntax er-macro-transformer
  (lambda (x)
    (syntax-case x ()
      ((k transformer) ;; transformer should be 3-ary: (exp rename compare)
       #'(lambda (input)
           (transformer
               (unwrap-syntax input)
               (lambda (sym) (datum->syntax #'k sym))
               free-identifier=?))))))

(define (make-inject-identifier id) ;; FIXME: use case-lambda
  (lambda in
    (if (null? in)
      (datum->syntax id (gensym))
      (datum->syntax id (car in)))))

(define-syntax er-macro-transformer+
  (lambda (x)
    (syntax-case x ()
      ((k transformer) ;; transformer should be 4-ary: 
                       ;; (exp rename compare inject)
       #'(lambda (input)
           (let ((exp (unwrap-syntax input)))
             (transformer
               exp
               (lambda (sym) (datum->syntax #'k sym))
               free-identifier=?
               (make-inject-identifier (car exp)))))))))

)
