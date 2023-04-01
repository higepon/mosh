(library (yuni util tables internal)
         (export
           define-converter1
           )

         (import 
           (for (rnrs) run expand))

(define-syntax genlookup-loop
  (lambda (x)
    (define (getidx fld sym)
      (define (itr idx cur)
        (if (pair? cur)
          (let ((a (car cur))
                (rest (cdr cur)))
            (if (eq? a sym)
              idx
              (itr (+ 1 idx) rest)))
          (syntax-violation #f "invalid argument" fld sym)))
      (itr 0 fld))
    (syntax-case x ()
      ((_ fields sym)
       (let* ((f (vector->list (syntax->datum #'fields)))
              (i (getidx f (syntax->datum #'sym))))
         (datum->syntax #'x i))))))

(define-syntax generate-field-lookup
  (syntax-rules ()
    ((_ fields sym)
     (lambda (x) (vector-ref x (generate-field-idx fields sym))))))

(define-syntax generate-field-idx
  (syntax-rules ()
    ((_ #(field-name0 ...) sym)
     (genlookup-loop #(field-name0 ...) sym))))


(define-syntax define-converter1
  (syntax-rules (=>)
    ((_ (name lookup => return pred) field-names field-data)
     (define (name x)
       (define get/lookup (generate-field-lookup field-names lookup))
       (define get/return (generate-field-lookup field-names return))
       (define (itr cur)
         (if (pair? cur)
           (let ((a (car cur))
                 (rest (cdr cur)))
             (if (pred (get/lookup a) x)
               (get/return a)
               (itr rest)))
           #f))
       (itr field-data)))))

)
