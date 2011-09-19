(library (yuni text makefile cmake-import)
         (export translate-cmake)
         (import (rnrs)
                 (shorten)
                 (mosh pp)
                 (srfi :8)
                 (yuni core)
                 (yuni util files)
                 (yuni text shell reader)
                 (yuni text makefile recipe)
                 (yuni text makefile translator)
                 (yuni text makefile expander)
                 (yuni text makefile reader))
;; globals 

; mymake: `make` command name
(define mymake "make")

;; utils
(define (make-command? l)
  (and (pair? l)
       (string? (car l))
       (let* ((cmd (car l))
              (cmdname (path-basename cmd)))
         (string=? mymake cmdname))))

(define (has-option? orig-l orig-str)
  (define (itr-has-option? l str) ;; => param
    (and (pair? l)
         (or (and (string? (car l)) (string=? (car l) str) 
                  (if (pair? (cdr l))
                    (cadr l)
                    (begin
                      (write (list 'ERR?-has-option? orig-l str) (current-error-port))
                      (newline (current-error-port))
                      "")))
             (itr-has-option? (cdr l) str))))
  (itr-has-option? orig-l orig-str))

(define (make-C? l) ;; => param
  ;; FIXME: not used?
  (and (make-command? l)
       (has-option? l "-C")))

(define (make-f? l) ;; => param
  (and (make-command? l)
       (has-option? l "-f")))

(define (make-target l) ;; => param
  (define (is-option? str)
    (and (string? str)
         (> (string-length str) 0)
         (let ((c (string-ref str 0)))
           (and (not (char=? #\- c))
                str))))
  (and (make-command? l)
       (let ((a (car (reverse l))))
         (or (is-option? a)
             "all"))))

(define (expand-command-template l)
  ;; FIXME: use match...
  (cond
    ((and (pair? l)
          (string? (car l))
          (string=? (car l) "cd"))
     (let ((m (if (string=? "/d" (cadr l)) ;; drop MinGW Make cd /d option
                (cons (car l) (cddr l))
                l)))
       (unless (eq? (caddr m) 'dand)
         (assertion-violation 'expand-template
                              "unknown setdir format"
                              l))
       `(setdir ,(cadr m) ,(expand-template (cdddr m)))))
    ((and (pair? l)
          (string? (car l))
          (string=? (car l) "echo"))
     `(echo ,(cadr l)))
    ((make-f? l) => (^[file]
                      `(make-rec ,file ,(make-target l))))
    (else l)))

(define (expand-redirect-template l)
  (define (conv sym)
    (case sym
      ((lbracket) 'input)
      ((rbracket) 'output)
      ((drbracket) 'output-append)
      (else #f)))
  ;; (cmd ... SYMBOL FILE) => (tem-SYMBOL FILE cmd ...)
  ;; SYMBOL/tem-SYMBOL := lbracket:input
  ;;                      rbracket:output
  ;;                      drbracket:output-append
  (or (and (pair? l)
           (> (length l) 2)
           (let* ((r (reverse l))
                  (sym (conv (cadr r)))
                  (file (car r)))
             (and sym
                  `(,sym ,file ,(expand-template (reverse (cddr r)))))))
      l))

(define (expand-template l)
  (expand-command-template (expand-redirect-template l)))

(define (recipe-dump1 r)
  (if (recipe? r)
    (if (recipe-phony? r)
      `(phony: ,(recipe-target r))
      `(,(recipe-target r) <= ,(recipe-deps r) ,@(recipe-commands r)))
    r))

(define (recipe*-dump l)
  (map recipe-dump1 l))

(define (translate-cmake path) ;; => (("filename" . recipe*) ...)
  (define mf (path-append path "Makefile"))
  (define env (list 
                (cons "COLOR" "")
                (cons "VERBOSE" "")
                (cons "MAKE" mymake)))
  (define root (makefile-translate path mf env))
  (define mf-cache (make-hashtable string-hash string=?))
  (define (scan-makefile root-path recipe*) 
    (define (shell-filter str)
      ;; chop "@" at first
      (or (and (not (string=? str ""))
               (char=? #\@ (string-ref str 0))
               (substring str 1 (string-length str)))
          str))
    (define (conv e)
      (let ((new-cmd* (map (^e (expand-template (shell-line-read (shell-filter e)))) 
                           (recipe-commands e))))
        (touch! e (cmd* new-cmd*))))
    (for-each conv recipe*))

  (define (translate/cached file)
    (let ((r (hashtable-ref mf-cache file)))
      (unless r
        (let ((mf (makefile-translate path file env)))
          (scan-makefile path mf)
          (hashtable-set! mf-cache file mf)))))

  (define (emit l)
    (define (proc e)
      (and (pair? e)
           (case (car e)
             ((make-rec)
              (let ((next (path-append path (cadr e))))
                (translate/cached next))))))
    (define (run recipe)
      (for-each proc (recipe-commands recipe)))
    (for-each run l))

  (scan-makefile path root)
  (hashtable-set! mf-cache mf root)
  (emit root)
  (receive (k v) (hashtable-entries mf-cache)
    (map (^[m h] (cons m h)) (vector->list k) (map recipe*-dump (vector->list v))))
  )

)
