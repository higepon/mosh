;; "Reduced" R6RS version (not supports bound/free identifier.)
(library (yuni transformer scheme identifier-object)
         (export identifier
                 syntax->datum
                 identifier-log
                 make-identifier identifier-rename
                 identifier?  free-identifier=?)
         (import (yuni core)
                 (yuni util lists)
                 (rnrs base))

(define* identifier
  (name library source-info log*))

(define (identifier? x)
  (is-a? x identifier))

(define* (free-identifier=? (id0 identifier) (id1 identifier))
  (let-with id0 (name library)
    (let-with id1 ((name1 name) (library1 library))
      (if (and library library1)
        (and (equal? library library1) (eq? name name1))
        (eq? name name1)))))

(define* (identifier-rename (id identifier) name library source-info reason)
  (let-with id (log*)
    (make identifier
          (name name)
          (library library)
          (source-info source-info)
          (log* (cons (cons reason id) log*)))))

(define* (identifier-log (id identifier))
  (let-with id (log*) log*))

(define (syntax->datum-core obj)
  (if (identifier? obj)
    (let-with obj (name) name)
    obj))

(define (syntax->datum obj)
  (sexp-map syntax->datum-core obj))

(define (make-identifier name library source-info)
  (make identifier
        (name name)
        (library library)
        (source-info source-info)
        (log* '())))

)
