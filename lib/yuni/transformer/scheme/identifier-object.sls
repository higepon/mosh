;; "Reduced" R6RS version (not supports bound/free identifier.)
(library (yuni transformer scheme identifier-object)
         (export identifier
                 syntax->datum
                 identifier-log
                 make-identifier identifier-rename
                 identifier-rename/id
                 identifier-name
                 free-identifier=?
                 identifier?)
         (import (yuni core)
                 (yuni util lists)
                 (rnrs base))

(define* identifier
  (name library log*))

(define (identifier? x)
  (is-a? x identifier))

(define* (free-identifier=? (id0 identifier) (id1 identifier))
  (let-with id0 (name library)
    (let-with id1 ((name1 name) (library1 library))
      (if (and library library1)
        (and (equal? library library1) (eq? name name1))
        (and (not library)
             (not library1)
             (eq? name name1))))))

(define* (identifier-rename (id identifier) name library reason)
  (let-with id (log*)
    (make identifier
          (name name)
          (library library)
          (log* (cons (cons id reason) log*)))))

(define* (identifier-rename/id (from identifier) (to identifier) reason)
  (let-with to (name library)
    (identifier-rename from name library reason)))

(define* (identifier-log (id identifier))
  (let-with id (log*) log*))

(define* (identifier-name (id identifier))
  (let-with id (name) name))

(define (syntax->datum-core obj)
  (if (identifier? obj)
    (identifier-name obj)
    obj))

(define (syntax->datum obj)
  (sexp-map syntax->datum-core obj))

(define (make-identifier name library log)
  (make identifier
        (name name)
        (library library)
        (log* (list (cons #f log)))))

)
