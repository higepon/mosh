(library (yuni util library-files)
         (export 
           library-set-code!
           library-name
           make-library-env
           search-library-bundle
           file->library-bundle
           symbol->path)
         (import
           (rnrs)
           (only (rnrs r5rs) quotient remainder)
           (yuni core)
           (yuni util files)
           ;(yuni util messages)
           )

(define-composite library-import
                  (name original-name library-name))
(define-composite library-export
                  (name original-name))
(define-composite library
                  (name exports imports path code))
(define-composite library-env
                  (search-path* flavor* library-file*))

(define* (library-name (l library))
  (let-with l (name) name))

(define (make-library-env)
  (make library-env
        (search-path* '())
        (flavor* '("nmosh" "mosh" "yuni"))
        (library-file* '())))

(define (set-search-path! env l)
  (touch! env (search-path* l)))
(define (get-search-path env)
  (~ env 'search-path*))

(define (append-search-path! env pth)
  (touch! env (search-path*
                (append search-path* (list pth)))))

;; legacy encoding for escaped paths based on URLencode
;; from nmosh
(define (symbol->path s)
  ;; convert a nibble (0-15) into 0-9a-f
  (define (nibblechar n)
    (cond
      ((<= 0 n 9) (integer->char (+ n #x30)))
      (else (integer->char (+ (- 10 n) #x61)))))
  (define (between? x y z) ; Gauche does not support 3 args char<=?
    (and (char<=? x y)
         (char<=? y z)))
  (define (convc c)
    (cond ;;from psyntax
      ((or (between? #\a c #\z)
           (between? #\A c #\Z)
           (between? #\0 c #\9)
           (memv c '(#\- #\. #\_ #\~))) (list c))
      (else (let ((i (char->integer c)))
              (list
                #\%
                (nibblechar (quotient i 16))
                (nibblechar (remainder i 16)))))))
  (list->string (apply append (map convc (string->list (symbol->string s))))))

(define (spec->path spec)
  (define (itr cur e)
    (string-append cur "/" (symbol->path e)))

  (let* ((truespec (drop-versions spec))
         (first (car truespec))
         (next (cdr truespec)))
    (fold-left itr (symbol->path first) next)))

(define (expand-suffix sep str sf*)
  (define (proc e)
    (string-append str sep e))
  (map proc sf*))

(define (expand-prefix sep str pr*)
  (define (proc e)
    (string-append e sep str))
  (map proc pr*))

(define (drop-versions n)
  (define (itr cur rest)
    (define (return) (reverse cur))
    (if (pair? rest)
      (let ((sym? (car rest))
            (next (cdr rest)))
        (if (symbol? sym?)
          (itr (cons sym? cur) next)
          (return)))
      (return)))
  (itr '() n))

(define (file->library-bundle pth)
  (define l (file->sexp-list pth))
  (define (proc prog)
    ;; bind clauses
    (let ((name (cadr prog))
          (export (caddr prog))
          (import (cadddr prog)))
      (make library
            (name (drop-versions name))
            (exports #f)
            (imports #f)
            (code #f)
            (path pth))))
  (define (itr cur prog)
    (if (pair? prog)
      (let ((lib? (car prog))
            (next (cdr prog)))
        (if (and (list? lib?) (eq? (car lib?) 'library))
          (itr (cons (proc lib?) cur) next)
          (itr cur next)))
      cur))
  (itr '() l))

(define (search-library-bundle env spec) ;; => (* library)
  (define core-name (spec->path spec))
  (define (append-suffix str)
    (expand-suffix "." str '("sls" "sps" "ss" "sch" "scm")))
  (define plain-path* (append-suffix core-name))
  (define flavored-path* 
    (fold-left append '()
    (map append-suffix (expand-suffix "." core-name (~ env 'flavor*)))))
  (define path-suffix* (append plain-path* flavored-path*))
  (define path* (fold-left append path-suffix*
                           (expand-prefix "/" path-suffix* (~ env 'search-path*))))
  (define (itr rest)
    (if (pair? rest)
      (let ((file (car rest))
            (next (cdr rest)))
        (display `(CHECK ,file))(newline)
        (cond
          ((file-exists? file)
           (let ((lib (file->library-bundle file)))
             (if lib lib (itr next))))
          (else (itr next))))
      #f))
  (itr path*))

)
