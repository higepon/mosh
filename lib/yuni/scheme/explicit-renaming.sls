;; MIT/GNU scheme compatible er-macro-transformer in R6RS syncase
(library (yuni scheme explicit-renaming)
         (export er-macro-transformer+/er
                 er-macro-transformer+
                 identifier?)
         (import (rnrs)
                 (for (yuni compat gensym) run expand)
                 (for (yuni scheme unwrap-syntax) run expand))
      
;; transformer should be 4-ary: 
;; (exp rename compare inject)
(define (er-macro-transformer+/er transformer rename-id)
  (lambda (input)
    (define (make-inject-identifier id) ;; FIXME: use case-lambda
      (lambda in
        (if (null? in)
          (datum->syntax id (gensym))
          (datum->syntax id (car in)))))
    (let* ((exp (unwrap-syntax input))
           (id (car exp))
           (rename (lambda (sym) (datum->syntax rename-id sym)))
           (compare (lambda (x y) (and (identifier? x)
                                       (identifier? y)
                                       (free-identifier=? x y))))
           (inject (make-inject-identifier id)))
      (transformer exp rename compare inject))))

(define (er-macro-transformer+ transformer)
  (lambda (input)
    (define (make-inject-identifier id) ;; FIXME: use case-lambda
      (lambda in
        (if (null? in)
          (datum->syntax id (gensym))
          (datum->syntax id (car in)))))
    (let* ((exp (unwrap-syntax input))
           (id (car exp))
           (rename (lambda (sym) (datum->syntax id sym)))
           (compare (lambda (x y) (and (identifier? x)
                                       (identifier? y)
                                       (free-identifier=? x y))))
           (inject (make-inject-identifier id)))
      (transformer exp rename compare inject))))


)
