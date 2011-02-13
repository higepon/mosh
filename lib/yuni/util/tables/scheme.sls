(library (yuni util tables scheme)
         (export
           table-metadata
           table-slots
           table/scheme
           table-map
           table-fold
           table-for-each
           table-metadata-ref
           file->table-list
           list->table)
         (import (rnrs)
                 (srfi :8)
                 (shorten)
                 (yuni util files)
                 (yuni core))

(define* table/scheme
  (metadata slots entry*))

(define (list->table l) ;; => table
  (define (make-metadata cur p)
    (if (pair? p)
      (let ((e (car p))
            (rest (cdr p)))
        (if (or (vector? e)
                (null? e))
          (reverse cur)
          (make-metadata (cons e cur) rest)))
      (reverse cur)))
  (define (slots+entries l)
    (if (pair? l)
      (let ((a (car l))
            (b (cdr l)))
        (if (vector? a)
          (values a b)
          (slots+entries b)))
      '()))
  (define (search-slots l)
    (receive (slots entries) (slots+entries l)
      slots))
  (define (search-entries l)
    (receive (slots entries) (slots+entries l)
      entries))
  (make table/scheme
        (metadata (make-metadata '() l))
        (slots (search-slots l))
        (entry* (map list->vector (search-entries l)))))

(define (file->table-list fn)
  (let ((f (file->sexp-list fn)))
    (map list->table f)))

(define* (table-metadata (tbl table/scheme))
  (let-with tbl (metadata) metadata))

(define* (table-slots (tbl table/scheme))
  (let-with tbl (slots) slots))

(define* (table-metadata-ref (tbl table/scheme) slot)
  (define (search l)
    (if (pair? l)
      (let ((e (car l))
            (rest (cdr l)))
        (if (pair? e)
          (let ((s (car e))
                (value (cdr e)))
            (if (eq? s slot)
              (if (and (list? value) (= 1 (length value)))
                (car value)
                value)
              (search rest)))
          (if (eq? e slot)
            #t
            (search rest))))
      #f))
  (let ((metadata (table-metadata tbl)))
    (search metadata)))

(define (syms->idx slots syms)
  (define (one sym)
    (define (itr idx cur)
      (if (pair? cur)
        (if (eq? (car cur) sym)
          idx
          (itr (+ 1 idx) (cdr cur)))
        #f))
    (itr 0 (vector->list slots)))
  (map one syms))

(define (make-lookup slots syms)
  (let ((idx* (syms->idx slots syms)))
    (^[entry]
      (define (one idx)
        (if (or (not idx) (>= idx (vector-length entry)))
          #f
          (vector-ref entry idx)))
      (map one idx*))))

(define (make-lookup/fold slots syms)
  (let ((idx* (syms->idx slots syms)))
    (^[cur entry]
      (define (one idx)
        (if (or (not idx) (>= idx (vector-length entry)))
          #f
          (vector-ref entry idx)))
      (let ((r (map one idx*)))
        ;; FIXME: use values...
        (cons cur r)))))

(define* (table-for-each (tbl table/scheme) syms proc)
  (let-with tbl (slots entry*)
    (let ((lookup (make-lookup slots syms)))
      (for-each (^e (apply proc (lookup e)))
                entry*))))

(define* (table-fold (tbl table/scheme) syms proc knil)
  (let-with tbl (slots entry*)
    (let ((lookup (make-lookup/fold slots syms)))
      (fold-left (^[cur e]
                   (apply proc (lookup cur e)))
                 knil
                 entry*))))

(define* (table-map (tbl table/scheme) syms proc)
  (reverse (table-fold tbl syms 
                       (^ m (let ((cur (car m))
                                  (param (cdr m)))
                              (cons (apply proc param) cur)))
                       '())))
)
