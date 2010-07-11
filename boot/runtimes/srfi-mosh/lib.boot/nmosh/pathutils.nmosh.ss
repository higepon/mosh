(library (nmosh pathutils)
         (export absolute-path?
                 relative-path?
                 simplify-path
                 path-append
                 expand-loadpath)
         (import (rnrs) (mosh))

;; from mosh-utils5.scm
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

(define do-absolute-path? 
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

(define (compose-rel-path l)
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
    (itr '() l))
  (apply string-append (insert-slash (omit-dot (omit-zerolen l)))))

(define (path->list pth)
  (strsep (pathfilter pth) #\/))

(define (expand-loadpath lp)
  (strsep lp CHR-ENVPATHSEP))

(define (path-append dir name) ;;FIXME: need canon.
  (string-append dir "/" name))

(define (absolute-path? pth)
  (do-absolute-path? (path->list pth)))

(define (relative-path? pth) (not (absolute-path? pth)))

(define (simplify-path pth)
  (define (make-simple base l)
    (define (chop-itr base-cur l-cur)
      (if (and (pair? base-cur) (pair? l-cur)
               (string=? (car base-cur) (car l-cur)))
        (chop-itr (cdr base-cur) (cdr l-cur))
        (values base-cur l-cur)))
    (define (run)
      (chop-itr base l))
    (define (return base l)
      (append
        (map (lambda (bogus) "..") base)
        l))
    (call-with-values run return))
  (cond
    ((absolute-path? pth)
     (let ((base (path->list (current-directory)))
           (l (path->list pth)))
       (let ((r (compose-rel-path (make-simple base l))))
         (if (< (string-length pth) (string-length r))
           pth
           r))))
    (else pth)))

)


