(define ERRPORT (current-error-port))
(define (PCK . obj)
  (if %verbose
    (begin 
      (display "-> " ERRPORT)
      (for-each (lambda (e)
		  (display e ERRPORT)
		  (display " " ERRPORT))
		obj)
      (newline ERRPORT))))

;;------------------------------------------------
;; definitions
;;------------------------------------------------

(define (run-win32-np?) (string=? "win32" (host-os)))
(define CHR-ENVPATHSEP (if (run-win32-np?) #\; #\:))

(define pathfilter 
  (if (run-win32-np?) 
    (lambda (str) 
      (and (string? str) 
	   (list->string (map (lambda (e) (if (char=? e #\\) #\/ e)) (string->list str)))))
    (lambda (str) str)))

(define pathfinish 
  (if (run-win32-np?)
    (lambda (str) (and (string? str) (list->string (cdr (string->list str)))))
    (lambda (str) str)))

(define (nmosh-cache-dir) 
  (let ((cpath (get-environment-variable "NMOSH_CACHEDIR")))
    (if cpath
      cpath
      (if (run-win32-np?) 
	(string-append (pathfilter (mosh-executable-path)) "cache") 
	(string-append (get-environment-variable "HOME") "/.nmosh-cache")))))

(define mosh-cache-dir nmosh-cache-dir)

(define (nmosh-cache-path) 
  (string-append (nmosh-cache-dir) "/"))

(define CACHEPATH (nmosh-cache-path))
(define CACHEDIR (nmosh-cache-dir))

(define absolute-path? 
  (if (run-win32-np?) ;FIXME: support UNC pathes
    (lambda (pl)
      (let ((a (car pl)))
	(and ; is a drive letter?
	  (= (string-length a) 2)
	  (char=? (cadr (string->list a)) #\:))))
    (lambda (pl) (= 0 (string-length (car pl))) )))

;;------------------------------------------------
;; utils
;;------------------------------------------------
(define (strsep str chr)
  (define (gather l) ;
    (define (itr cur rest0 rest1)
      (cond
	((not (pair? rest1)) (reverse cur))
	(else
	  (itr (cons (substring str
		       (+ 1 (car rest0)) 
		       (car rest1)) cur) 
	       (cdr rest0) 
	       (cdr rest1)))))
    (itr '() l (cdr l)))
  (define (spl l s)
    (define (itr idx cur rest)
      (cond
	((not (pair? rest)) (reverse (cons idx cur)))
	((char=? s (car rest))
	 (itr (+ idx 1) (cons idx cur) (cdr rest)))
	(else
	  (itr (+ idx 1) cur (cdr rest)))))
    (itr 0 (list -1) l))
  (if (string? str)
    (let* ((l (string->list str))
	   (m (spl l chr))
	   (r (gather m)))
      r )
    '()
    ))
;;------------------------------------------------
;; path handling
;;------------------------------------------------
(define RUNPATH (pathfilter (current-directory)))

(define (compose-path l)
  (define (fold-dotdot l)
    (define (itr cur rest)
      (if (pair? rest)
	(let ((a (car rest)))
	  (if (string=? ".." a)
	    (itr (cdr cur) (cdr rest)) ; drop top
	    (itr (cons a cur) (cdr rest))))
	(reverse cur)))
    (itr '() l))
  (define (omit-dot l)
    (define (itr cur rest)
      (if (pair? rest)
	(let ((a (car rest)))
	  (if (string=? "." a)
	    (itr cur (cdr rest)) ; drop "."
	    (itr (cons a cur) (cdr rest))))
	(reverse cur)))
    (itr '() l))
  (define (omit-zerolen l)
    (define (itr cur rest)
      (if (pair? rest)
	(let ((a (car rest)))
	  (if (= 0 (string-length a))
	    (itr cur (cdr rest))
	    (itr (cons a cur) (cdr rest))))
	(reverse cur)))
    (itr '() l))
  (define (insert-slash l)
    (define (itr cur rest)
      (if (pair? rest)
	(itr (cons "/" (cons (car rest) cur)) (cdr rest))
	(reverse (cdr cur)))) ;drop last "/"
    (itr (list "/") l))

  ;(PCK 'COMPOSING: l)
  (apply string-append (insert-slash (fold-dotdot (omit-dot (omit-zerolen l))))))

(define (make-absolute-path pth)
  (let ((pl (strsep (pathfilter pth) #\/)))
    (if (pair? pl)
      (pathfinish
	(compose-path (if (absolute-path? pl)
			(if (string=? "win32" (host-os)) ;FIXME FIXME FIXME!
			  pl ; Include Drive letter
			  (cdr pl)) ; Dispose heading /
			(append (strsep RUNPATH #\/) pl))))
      "")))

(define (pathsep str)
  (strsep str CHR-ENVPATHSEP))

; FIXME: handle exceptions
(define tmpname-base (ex:unique-token))
(define tmpname-counter 0)

(define (gen-tmpname)
  (set! tmpname-counter (+ 1 tmpname-counter))
  (string-append CACHEPATH tmpname-base (number->string tmpname-counter)))

(define (ca-writeobj fn obj)
  (when (file-exists? fn) (delete-file fn))
  (let ((tmpfilename (gen-tmpname)))
    (call-with-port (open-file-output-port tmpfilename) (lambda (p) (fasl-write! obj p)))
    (rename-file tmpfilename fn) ; MUST be on same file system
    (PCK 'CACHE "cache-write" tmpfilename '=> fn)))

(define (ca-readobj fn)
  (call-with-port (open-file-input-port fn) fasl-read))

; load cache or generate cache (and load them)
(define (ca-filename->cachename fn)
  (define (escape str)
    (list->string (map (lambda (e) (cond
				     ((char=? e #\:) #\~)
				     ((char=? e #\/) #\~)
				     ((char=? e #\~) #\@)
				     (else e)))
		       (string->list str))))
  (string-append CACHEPATH (escape fn) ".nmosh-cache"))

(define dbg-files '())
(define (dbg-addfile fn cdn dfn)
  (set! dbg-files (cons (list fn dfn) dbg-files)))
(define dbg-syms '())
(define (dbg-addsyms syms)
  (when syms (set! dbg-syms (append syms dbg-syms))))

(define (ca-scandeps-begin code)
  (fold-left (lambda (cur e)
	       (if (not (pair? e))
		 cur
		 (cond
		   ((eq? 'begin (car e))
		    (append cur (ca-scandeps-begin (cdr e))))
		   (else
		     (let ((r (ca-scandeps-unit e)))
		       (if r
			 (cons r cur)
			 cur))))))
	     '()
	     code))

(define (ca-compose-libimports p)
  (let ((libnames (cadar p))
	(builds (cadadr p)))
    (map (lambda (name build) (cons (car name) build)) libnames builds)))

(define (ca-scandeps-unit code)
  (if (not (pair? code))
    #f
    (case (car code)
      ((ex:import-libraries-for-run) (ca-compose-libimports (cdr code)))
      ((ex:register-library!) (ca-scandeps-unit (cadr code)))
      ((ex:make-library) (ca-compose-libimports (cddddr code)))
      (else #f))))

(define (ca-scandeps code)
  (apply append 
	 (apply append (map (lambda (e) (ca-scandeps-begin e)) code))))

(define (ca-makecache code compiled-code syms fn cfn dfn name depfiles)
  (ca-prepare-cache-dir)
  (let ((deps (ca-scandeps code)))
    (PCK 'DEPS deps)
    (PCK 'DEPFILES depfiles)
    (ca-writeobj cfn (list
		       (cons 'MOSH-CODE compiled-code)
		       (cons 'DEPS (cons fn deps))
                       (cons 'DEPFILES depfiles))))
  (when dfn
    (ca-writeobj dfn (list
		       (cons 'DBG-FILENAME fn)
		       (cons 'DBG-SOURCE code)
		       (cons 'DBG-SYMS syms)))))

(define (ca-read-all fn)
  (define (itr cur p)
    (let ((r (read p)))
      (if (eof-object? r)
	(reverse cur)
	(itr (cons r cur) p))))
  (call-with-input-file fn (lambda (p) (itr '() p))))

(define (ca-expand/compile-for-cache fn)
  (define (step save? run? code cur-code cur-compiled-code cur-syms cur-deps)
    (PCK "Expanding..")
    (let* ((ex (ex:expand-sequence/debug code #f))
	   (ex-code (car ex))
	   (ex-syms (cadr ex))
           (ex-save? (caddr ex))
           (ex-deps (map make-absolute-path (cadddr ex))))
      (PCK 'CACHE: 'COMPILE...)
      (let ((co (compile-w/o-halt (cons 'begin ex-code))))
	(ca-load-compiled-code (list (cons run? co)) ex-syms)
	(list 
          (and save? ex-save?)
          (cons ex-code cur-code) 
          (cons (cons run? co) cur-compiled-code) 
          (append ex-syms cur-syms)
          (append ex-deps cur-deps)))))
  (define (proc save? code cur-code cur-compiled-code cur-syms cur-deps)
    (cond 
      ((and (pair? code) (pair? (car code)) (eq? 'library (caar code)))
       (let* ((s (step save? #t (list (car code)) 
                       cur-code cur-compiled-code cur-syms cur-deps))
              (nx-save? (car s))
	      (nx-code (cadr s))
	      (nx-compiled-code (caddr s))
	      (nx-syms (cadddr s))
              (nx-deps (car (cddddr s))))
	 (proc nx-save? (cdr code) nx-code nx-compiled-code nx-syms nx-deps))) ;when library
      ((pair? code)
       (step save? #f code cur-code cur-compiled-code cur-syms cur-deps)) ; when program
      (else
	(list save? cur-code cur-compiled-code cur-syms cur-deps)))) ; library only file
  (PCK "Reading" fn)
  (let ((code (ca-read-all fn)))
    (proc #t code '() '() '() '())))

(define (ca-need-update?-check cfn d)
  ;; check library state
  ;; d = list of (LIBNAME . BUILD) | FILENAME
  (define (checkfile e)
    (and (string? e)
         (if (file-exists? e)
           (file-newer? e cfn)
           #t)
	 (begin (PCK 'CACHE: "source changed" e) #t)))
  (define (checklib e)
    (and (pair? e)
	 (let ((libname (car e))
	       (build (cdr e)))
	   (let ((lib (ex:lookup-library libname #f)))
	     (and 
	       (not (eq? build (ex:library-build lib)))
	       (begin (PCK 'CACHE: "library build changed" libname build '=> (ex:library-build lib)) #t))))))
  (or (find checkfile d) (find checklib d)))

(define (ca-need-update? obj cfn) 
  (define (fil x)
    (if x x '()))
  (let ((d (assq 'DEPS obj))
        (f (assq 'DEPFILES obj)))
    (if (and d f)
      (ca-need-update?-check cfn (append (fil d) (fil f)))
      #t ; old version of nmosh-cache
      )))

(define (for-each1-tail proc lst)
  (cond
    ((and (pair? lst) (pair? (cdr lst)))
     (proc (car lst))
     (for-each1-tail proc (cdr lst)))
    ((and (pair? lst) (null? (cdr lst)))
     (proc (car lst)))))
    
(define (ca-runcache obj) ; run both lib/prog
  (let ((m (assq 'MOSH-CODE obj)))
      (for-each1-tail (lambda (e) (eval-compiled! (cdr e)))
		(cdr m))))

(define (ca-load-compiled-code code syms)
  (PCK "evaluating...")
  (dbg-addsyms syms)
  (for-each1-tail (lambda (e) 
		    (when (car e)
		      (eval-compiled! (cdr e)))) code))

(define (ca-load-compiled-code/run code syms)
  (PCK "evaluating(run)...")
  (dbg-addsyms syms)
  (for-each1-tail (lambda (e) 
		    (eval-compiled! (cdr e))) code))

(define (ca-load/cache rfn recompile? name)
  (let* ((fn (make-absolute-path rfn))
	 (cfn (ca-filename->cachename fn))
	 (dfn (string-append cfn ".ndbg")))
    (dbg-addfile fn cfn dfn)
    (cond
      ((file-exists? cfn)
       (cond ((and (not recompile?) (file-newer? cfn fn))
	      (PCK 'CACHE: 'loading.. cfn)
	      (let ((obj (ca-readobj cfn)))
		(cond
		  ((ca-need-update? obj cfn)
		   (PCK 'CACHE: 'RECOMPILE!! cfn)
		   (ca-load/cache fn #t name))
		  (else
		    (PCK "Loading code..." name)
		    (ca-runcache obj)))))
	     (else
	       (PCK 'CACHE: 're-cache..)
	       (delete-file cfn)
	       (ca-load/cache fn #t name))))
      (else
	(PCK 'CACHE: 'loading fn)
	(let* ((c (ca-expand/compile-for-cache fn))
               (save? (car c))
	       (code (cadr c))
	       (compiled-code (caddr c))
	       (syms (cadddr c))
               (deps (car (cddddr c))))
          (cond
            (save?
              (PCK "Writing to" cfn)
              (ca-makecache (reverse code) (reverse compiled-code) syms fn cfn dfn name deps))
            (else
              (PCK "Cache was not saved due to user reqest")))
          (when (and (pair? compiled-code) 
                     (pair? (car compiled-code)) 
                     (not (caar compiled-code))) ; when this cache contains a program
            (ca-load-compiled-code/run (list (cons #t (cdar compiled-code))) '())))))))

(define (ca-load/disable-cache fn)
  (PCK "Loading" fn "(ACC disabled)")
  (let* ((c (ca-expand/compile-for-cache fn))
	 (code (cadr c))
	 (compiled-code (caddr c))
	 (syms (cadddr c)))
    (ca-load-compiled-code/run compiled-code syms)))

(define (ca-load fn recompile? name)
  (cond
    (%disable-acc 
      (ca-load/disable-cache fn))
    (else (ca-load/cache fn recompile? name))))

(define (ca-prepare-cache-dir)
  (unless (file-exists? CACHEDIR)
    (create-directory CACHEDIR)))

(define (make-prefix-list)
  (define (append-prefix-x l str)
    (cond
      ((and str (file-exists? str))
       (map (lambda (e) (string-append (pathfilter str) "/" e)) l))
      (else '())))
  (define (append-prefix-l l lstr)
    (define (itr cur rest)
      (cond
	((not (pair? rest)) cur)
	(else
          ;(PCK 'append-prefix-itr (car rest))
	  (itr (append cur (append-prefix-x l (car rest))) (cdr rest)))))
    (itr '() lstr))

  (define (append-prefix-curpath l)
    (append-prefix-x l (current-directory)))
  (define (append-prefix-execpath l)
    (append-prefix-x l (mosh-executable-path)))
  (define (append-prefix-stdlibpath l)
    (append-prefix-x l (standard-library-path)))
  (define (append-prefix-loadpath l)
    (let ((var (get-environment-variable "MOSH_LOADPATH")))
      ;(PCK 'var var)
      (append-prefix-l l (pathsep var))))
  (define (append-prefix l)
    (append
      (append-prefix-execpath l)
      (if %loadpath (append-prefix-l l (pathsep %loadpath)) '())
      (if (get-environment-variable "MOSH_LOADPATH")
        (append-prefix-loadpath l)
	'())
      (append-prefix-curpath l)
      (append-prefix-stdlibpath l)
      l ;fallback
      ))
  (append-prefix (list "" "lib/")))

(define prefix-list (make-prefix-list))

; TODO: support multiple libraries
(define (library-name->filename name) ; => PATH or #f
  (define (expand-prefix str l)
    (map (lambda (e) (string-append e str)) l))
  (define (expand-suffix str l)
    (map (lambda (e) (string-append str e)) l))
  (define (nibblechar n)
    (cond
      ((<= 0 n 9) (integer->char (+ n #x30)))
      (else (integer->char (+ (- 10 n) #x61)))))
  (define (between? x y z) ; Gauche does not support 3 args char<=?
    (and (char<=? x y)
	 (char<=? y z)))
  (define (namesymbol s)
    (define (convc c)
      (cond ;;from psyntax
	((or (between? #\a c #\z)
	     (between? #\A c #\Z)
	     (between? #\0 c #\9)
	     ;(append-prefix-curpath l)
	     (memv c '(#\- #\. #\_ #\~))) (list c))
	(else (let ((i (char->integer c)))
		(list
		  #\%
		  (nibblechar (quotient i 16))
		  (nibblechar (remainder i 16)))))))
    (list->string (apply append (map convc (string->list (symbol->string s))))))

  (define (check-files l)
    (if (null? l)
      #f
      (begin
        ;(PCK 'check-file (car l))
	(if (file-exists? (car l))
	  (car l)
	  (check-files (cdr l))))))
  ; e.g. for (example test) :
  ; 1. build base (example test) => "example/test"
  (define (basename name)
    (define (itr cur l)
      (if (null? l)
	cur
	(itr (string-append cur "/" (namesymbol (car l))) (cdr l))))
    (itr (namesymbol (car name)) (cdr name)))
  ; 2. expand pre/sufx
  (define (expand-name name)
    (apply append
	   (map (lambda (s) (expand-suffix s 
					   '(".nmosh.sls" ".nmosh.ss" ".nmosh.scm"
					     ".mosh.sls" ".mosh.ss" ".mosh.scm"
					     ".sls" ".ss" ".scm")))
		(expand-prefix name prefix-list))))

  (let* ((fn (check-files (expand-name (basename name))))
	 (cfn (make-absolute-path fn)))
    (if fn
      (begin
	;(PCK 'PATH fn '=> cfn)
	cfn)
      #f))
  )


