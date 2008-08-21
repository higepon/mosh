#!/usr/bin/env gosh
(use srfi-1)
(use srfi-43)
(use file.util)
(use gauche.sequence)
(use gauche.parseopt)

(define vector->list-table (make-hash-table 'eq?))

(define (my-vector->list v)
  (if (hash-table-get vector->list-table v #f)
      (hash-table-get vector->list-table v #f)
      (begin
        (let1 lst (vector->list v)
          (hash-table-put! vector->list-table v lst)
          lst))))

(define (flatten lst)
  (cond
   [(null? lst) '()]
   [(pair? lst)
    (append (flatten (car lst)) (flatten (cdr lst)))]
   [(vector? lst)
    (flatten (my-vector->list lst))]
   [else (list lst)]))


(define (collect-list lst)
  (define (rec lst)
    (cond
     [(null? lst) '()]
     [(number? (car lst))
      (rec (cdr lst))]
     [(and (pair? (car lst)) (eq? (caar lst) '*insn*))
      (rec (cdr lst))]
     [(and (pair? (car lst)) (eq? (caar lst) '*compiler-insn*))
      (rec (cdr lst))]
     [(null? (car lst))
      (rec (cdr lst))]
     [(vector? (car lst))
      (let1 vlist (my-vector->list (car lst))
        (if (null? vlist)
            (rec (cdr lst))
            (append (list vlist) (rec vlist) (rec (cdr lst)))))]
     [(list? (car lst))
      (append (list (car lst)) (rec (car lst)) (rec (cdr lst)))]
     [else
      (rec (cdr lst))]))
  (reverse (rec lst)))

(define ht (make-hash-table))
(define inc 0)

;; Definitions of Mosh Object memory layout.

(define (make-int n)
  (+ (ash n 2) 1))

(define (make-instruction n)
  (+ (ash n 5) 14))

(define (make-compiler-instruction n)
  (+ (ash n 5) 30))

(define (make-const n)
  (+ (ash n 4) 6))

(define (make-char c)
  (+ (ash c 3) 2))

(define (make-nil)
  (make-const 0))

(define (make-true)
  (make-const 4))

(define (make-false)
  (make-const 5))

;; シンボル名をコメントで挿入する

;; symbol can include */ that miss closes C++ comment tag.
(define (escape-symbol symbol)
  (string->symbol (regexp-replace-all #/\// (symbol->string symbol) "\\\\")))

(define (vector->cpp name obj)
  (define symbols  (collect-all-symbols))
  (define (rec obj)
    (cond
     [(symbol? obj)
      (receive (index o) (find-with-index (lambda (x) (eq? x obj)) symbols)
        (unless o
          (error "symbol not found. May be missing of dependencies on Makefile.am " obj))
        (format "        builtinSymbols[~d].val /* ~a */" index (escape-symbol (list-ref symbols index))))]
     [(number? obj)
      (format "        ~d /* ~d */" (make-int obj) obj)]
     [(and (pair? obj) (eq? (car obj) '*insn*))
      (format "        ~d /* *insn* */" (make-instruction (second obj)))]
     [(and (pair? obj) (eq? (car obj) '*compiler-insn*))
      (format "        ~d /* *compiler-insn* */" (make-compiler-instruction (second obj)))]
     [(regexp? obj)
      (format "             Object::makeRegexp(UC(~s)).val" (regexp->string obj))]
     [(string? obj)
      (if (string=? obj "\n")
          (format "        Object::makeString(UC(\"\\n\")).val")
          (format "        Object::makeString(UC(~s)).val" obj))]
     [(vector? obj)
      (cond
       [(zero? (vector-length obj))
        "        Object::makeVector(0).val"]
       [else
        (let ([lst (my-vector->list obj)])
          (format "            Object::makeVector(~a).val" (hash-table-get ht lst)))])]
     [(char? obj)
      (cond
       [(char=? obj #\space)
        (format "            ~d /* ~a */" (make-char (char->integer #\space)) "#\space")]
       [(char=? obj #\newline)
        (format "            ~d /* ~a */" (make-char (char->integer #\newline)) "#\newline")]
       [else
        (format "            ~d /* ~a */" (make-char (char->integer obj)) obj)])]
     [(boolean? obj)
      (format "        ~d /* ~a */" (if obj (make-true) (make-false)) obj)]
     [(null? obj) (format "        ~d /* ~a */" (make-nil) obj)]
     [(list? obj)
      (if (hash-table-get ht obj #f)
          (format "        ~a" (hash-table-get ht obj #f))
          (let ([array-name (format "array~d" inc)]
                [list-name (format "list~d" inc)]
                [len (length obj)])
            (hash-table-put! ht obj (string-append list-name ".val"))
            (let1 v (format "    static word ~a[] = {\n~a\n    };\n    Object ~a = Pair::wordArrayToList(~a, ~d);\n"
                            array-name (string-join (map rec obj) ",\n") list-name array-name len)
            (set! inc (+ inc 1))
            v)
            ))]
     [(pair? obj)
      (format "    Object::cons(Object::makeRaw(~a), Object::makeRaw(~a)).val" (rec (car obj)) (rec (cdr obj)))]
     [else
      (error "unknown Object")]))
  (define (make-obj i obj)
    (rec obj))
  (print "#include \"Builtin.h\"\n")
  (print "using namespace scheme;\n")
  (format #t "Object scheme::~a() {\n" name)
  (print "    const Object* builtinSymbols = getBuiltinSymbols();")
  (let* ([source (vector->list obj)]
         [collected (collect-list source)])
    (for-each (lambda (src) (print (rec src))) collected)
    (print (rec source))
  (format #t "    return Object::makeVector(list~d);\n}\n" (length collected))))

(define (collect-symbol-from-file file)
  (delete-duplicates
   (append-map (lambda (sexp)
                 (filter symbol? (flatten (vector->list sexp))))
               (file->sexp-list file))))

(define (collect-all-symbols)
  (sort
   (delete-duplicates
    (append-map collect-symbol-from-file (sys-glob "*.scmc")))
   (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (print-symbol-header symbols)
  (print "#include \"Builtin.h\"\n")
  (print "using namespace scheme;\n")
  (print "const Object* scheme::getBuiltinSymbols() {")
  (print "    static const Object builtinSymbols[] = {")
  (for-each
   (lambda (symbol) (format #t "        SA(\"~a\"),\n" symbol))
   symbols)
  (print "    };")
  (print "    return builtinSymbols;")
  (print "}"))

(define (main args)
  (let-args (cdr args)
      ([generate-symbol-header? "g" #f])
    (cond
     [generate-symbol-header?
      (format #t "// Do not edit this file generated by ~a.\n" (first args))
       (print-symbol-header (collect-all-symbols))]
     [else
      (format #t "// Do not edit this file generated by ~a.\n" (first args))
      (with-input-from-file (third args)
        (lambda ()
          (let1 obj (read)
            (if (vector? obj)
                (vector->cpp (second args) obj)
                (errorf "~a : vector required, but got ~a" (first args) obj)))))
      ]))
  0)
