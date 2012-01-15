(define ERRPORT (current-error-port))
(define DEBUGGING #f)
(define (PCK . obj)
  (if %verbose
    (begin 
      (if (not DEBUGGING)
        (begin 
          (display "-> " ERRPORT)
          (for-each (lambda (e)
                      (display e ERRPORT)
                      (display " " ERRPORT))
                    obj)
          (newline ERRPORT))))))

(define (DEBUGMODE-ON)
  (set! DEBUGGING #t))

;;------------------------------------------------
;; library aliasing
;;------------------------------------------------
(define library-rename-table '())
(define (set-library-rename-table! spec)
  (set! library-rename-table spec))

(define (rename-library spec)
  (let ((x (assoc spec library-rename-table)))
    (cond
      (x 
        (let ((newname (cdr x)))
          (PCK "alias: " spec "=>" newname)
          newname))
      (else 
        ;(PCK "not renamed: " spec)
        spec))))

;;------------------------------------------------
;; definitions
;;------------------------------------------------

(define (run-win32-np?) (string=? "win32" (host-os)))
(define CHR-ENVPATHSEP (if (run-win32-np?) #\; #\:))

(define pathfilter 
  (if (run-win32-np?) 
    (lambda (str) 
      (and (string? str) 
           (list->string (map (lambda (e) (if (char=? e #\\) #\/ e)) 
                              (string->list str)))))
    (lambda (str) str)))

(define pathfinish 
  (if (run-win32-np?)
    (lambda (str) (and (string? str) (list->string (cdr (string->list str)))))
    (lambda (str) str)))

;; Win32 path hack.

(define (make-extended-path str)
  ;; Win32 extened path won't allow slash as path-sep.
  ;; So we map them to back-slashes
  (string-append "\\\\?\\"
                 (list->string
                   (map (lambda (c) (if (char=? c #\/) #\\ c))
                        (string->list str)))))

(define path-absolute
  (if (run-win32-np?)
    (lambda (str)
      (let ((head (and (< 1 (string-length str))
                       (substring str 0 3))))
        (if (and head (string=? "\\\\" head)
                 (not (char=? #\: (string-ref head 1))))
          str ;; return the path as-is if the path wasn't a absolute local path.
          (make-extended-path str))))
    (lambda (str) str)))

(define (nmosh-cache-dir) 
  (let ((cpath (get-environment-variable "NMOSH_CACHEDIR")))
    (if cpath
      cpath
      (if %nmosh-portable-mode
        (string-append (pathfilter (mosh-executable-path)) "cache") 
        (if (run-win32-np?) 
          (let ((h (get-environment-variable "LOCALAPPDATA")))
            (if h
              (string-append h "\\nmosh-cache")
              (let ((i (get-environment-variable "APPDATA")))
                (if i
                  (string-append i "\\nmosh-cache")
                  #f))))
          (let ((h (get-environment-variable "HOME")))
            (if h
              (string-append h "/.nmosh-cache")
              #f)))))))

(define mosh-cache-dir nmosh-cache-dir)

(define (nmosh-cache-path) 
  (let ((c (nmosh-cache-dir)))
    (if c
      (string-append c "/")
      #f)))

;; CACHEPATH includes trailing slash (or back-slash on Win32)
(define CACHEPATH (path-absolute (nmosh-cache-path)))
(define CACHEDIR (nmosh-cache-dir))

(define has-cachepath? (and CACHEPATH CACHEDIR))

(define absolute-path? 
  (if (run-win32-np?)
    (lambda (pl)
      (let ((a (car pl)))
        (or
          (and ; is a drive letter?
            (= (string-length a) 2)
            (char=? (cadr (string->list a)) #\:))
          ;; ... or UNC path ?
          (= (string-length a) 0))))
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
  (apply string-append 
         (insert-slash (fold-dotdot (omit-dot (omit-zerolen l))))))

(define make-absolute-path
  (if (run-win32-np?)
    (lambda (pth)
      (define pl (strsep (pathfilter pth) #\/))
      (cond
        ((and
           pth
           (< 4 (string-length pth))
           (string=? "\\\\?\\" (substring pth 0 4)))
         ;; If it was extended path, return it as-is
         pth)
        (else
          (if (pair? pl) 
            (path-absolute
              (pathfinish
                (compose-path
                  ;; Standard absolute path (c:\hoge\fuga)
                  (if (and (< 2 (string-length pth))
                           (char=? #\: (string-ref pth 1)))
                    pl
                    (append (strsep RUNPATH #\/) pl)))))
            ""))))
    (lambda (pth)
      ;; FIXME: To avoid psyntax-mosh bug
      (define pl (strsep (pathfilter pth) #\/))
      (if (pair? pl)
        (path-absolute
          (pathfinish
            (compose-path
              (if (absolute-path? pl)
                (cdr pl)
                (append (strsep RUNPATH #\/) pl)))))
        ""))))

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
    (call-with-port (open-file-output-port tmpfilename) 
                    (lambda (p) (fasl-write! obj p)))
    (rename-file tmpfilename fn) ; MUST be on same file system
    (PCK 'CACHE "cache-write" tmpfilename '=> fn)))

;; FIXME: version support (from 0.2.7)
;; 0.2.7 cache doesn't have LONGNAME field
(define (ca-readobj bfn fn)
  (call-with-port (open-file-input-port fn) 
                  (lambda (p)
                    (define obj (fasl-read p))
                    (and (eq? (caar obj) 'LONGNAME)
                         (string=? (cdar obj) bfn)
                         obj))))

; load cache or generate cache (and load them)
(define pathchop
  (if (run-win32-np?)
    (lambda (pth)
      (define l (string-length pth))
      (or
        (and (< 3 l)
             (string=? "\\\\?\\"
                       (substring pth 0 4))
             (substring pth 4 l))
        pth))
    (lambda (pth) pth)))

(define (ca-filename->cachename fn)
  (define (escape str)
    (list->string (map (lambda (e) (cond
                                     ((char=? e #\:) #\~)
                                     ((char=? e #\\) #\~)
                                     ((char=? e #\/) #\~)
                                     ((char=? e #\~) #\@)
                                     (else e)))
                       (string->list str))))
  (string-append (escape (pathchop fn)) ".nmosh-cache"))

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

(define (cachename-shorten fn)
  (define l (string-length fn))
  (if (< 200 l)
    (string-append "@" (substring fn (- l 200) l)) 
    fn))

;; NB: LONGNAME must be first
(define (ca-makecache code compiled-code syms fn bfn cfn dfn name depfiles)
  (ca-prepare-cache-dir)
  (let ((deps (ca-scandeps code)))
    (PCK 'DEPS deps)
    (PCK 'DEPFILES depfiles)
    (ca-writeobj cfn (list
                       (cons 'LONGNAME bfn)
                       (cons 'MOSH-CODE compiled-code)
                       (cons 'DEPS (cons fn deps))
                       (cons 'DEPFILES depfiles))))
  (when dfn
    (ca-writeobj dfn (list
                       (cons 'LONGNAME bfn)
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
    (let* ((ex (ex:expand-sequence/debug fn code #f))
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
       ;when library
       (let* ((s (step save? #t (list (car code)) 
                       cur-code cur-compiled-code cur-syms cur-deps))
              (nx-save? (car s))
              (nx-code (cadr s))
              (nx-compiled-code (caddr s))
              (nx-syms (cadddr s))
              (nx-deps (car (cddddr s))))
         (proc nx-save? (cdr code) nx-code nx-compiled-code nx-syms nx-deps))) 
      ((pair? code)
       ; when program
       (step save? #f code cur-code cur-compiled-code cur-syms cur-deps)) 
      (else
        ; library only file
        (list save? cur-code cur-compiled-code cur-syms cur-deps)))) 
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
               (begin 
                 (PCK 'CACHE: "library build changed" 
                      libname build '=> (ex:library-build lib)) #t))))))
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
         (bfn (ca-filename->cachename fn))
         (cfn (string-append CACHEPATH (cachename-shorten bfn)))
         (dfn (string-append cfn ".ndbg")))
    (dbg-addfile fn cfn dfn)
    (cond
      ((file-exists? cfn)
       (cond ((and (not recompile?) (file-newer? cfn fn))
              (PCK 'CACHE: 'loading.. cfn)
              (let ((obj (ca-readobj bfn cfn)))
                (cond
                  ((not obj)
                   (PCK "CACHE: WARNING: Cache name conflict!")
                   (ca-load/cache fn #t name))
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
              (ca-makecache (reverse code) (reverse compiled-code) syms fn bfn cfn dfn name deps))
            (else
              (PCK "Cache was not saved due to user reqest")))
          (when (and (pair? compiled-code) 
                     (pair? (car compiled-code)) 
                     (not (caar compiled-code))) 
            ; when this cache contains a program
            (ca-load-compiled-code/run 
              (list (cons #t (cdar compiled-code))) '())))))))

(define (ca-load/disable-cache fn)
  (PCK "Loading" fn "(ACC disabled)")
  (let* ((c (ca-expand/compile-for-cache fn))
         (code (cadr c))
         (compiled-code (caddr c))
         (syms (cadddr c)))
    (ca-load-compiled-code/run compiled-code syms)))

(define (ca-load fn recompile? name)
  (cond
    ((or (not has-cachepath?) %disable-acc)
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
  (define (append-prefix-execpath-rel l)
    (let ((pth (mosh-executable-path)))
      (if pth
        (append-prefix-x l (string-append pth (standard-library-path)))
        '())))
  (define (append-prefix-stdlibpath l)
    (append-prefix-x l (standard-library-path)))
  (define (append-prefix-loadpath l)
    (let ((var (get-environment-variable "MOSH_LOADPATH")))
      ;(PCK 'var var)
      (append-prefix-l l (pathsep var))))
  (define (append-prefix l)
    (append
      ;(append-prefix-execpath l)
      (if %loadpath (append-prefix-l l (pathsep %loadpath)) '())
      (if (get-environment-variable "MOSH_LOADPATH")
        (append-prefix-loadpath l)
        '())
      (append-prefix-curpath l)
      (if %nmosh-prefixless-mode '() (append-prefix-stdlibpath l))
      l ;fallback
      (append-prefix-execpath-rel l) ; fallback
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
    ; ... But this is no longer true for Gauche 0.9.2
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
        ;(PCK 'check-file (make-absolute-path (car l)))
        (if (file-exists? (make-absolute-path (car l)))
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
           (map (lambda (s) 
                  (expand-suffix s 
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


