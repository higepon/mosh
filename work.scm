(import (rnrs)
        (mosh)
        (srfi :1)
        (srfi :26)
        (srfi :98))

(include "./scripts/r6rs-symbols.dat")

(define r6rs-dir (string-append (get-environment-variable "HOME") "/Desktop/r6rs/document/"))
(define out-dir (string-append (get-environment-variable "HOME") "/mosh/doc/text/"))

(define files '(("base.tex"
                 1
                 "Base Library"
                 ("(rnrs base (6))")
                 "The (rnrs base (6)) library, which exports many of the procedure and syntax bindings that are traditionally associated with Scheme.")
                
                ))


(define (adjust-args args-string)
  (if args-string
      (map string->symbol
           (map (lambda (x)
                  (cond
                   [(string=? x "\\dotsfoo") "..."]
                   [(string=? x "$\\ldots$") "..."]
                   ;; hyper{test} => test
                   ;; hyperi{exp} => exp1
                   ;; hyperii{exp} => exp2
                   [(#/hyper(i*){(.*)}/ x) => (lambda (m)
                                                (if (m 1) ;; count i
                                                    (format "~a~a" (m 2) (string-length (m 1)))
                                                    (m 2)))]
                   [(#/var(i*){(.*)}/ x) => (lambda (m)
                                              (if (m 1) ;; count i
                                                  (format "~a~a" (m 2) (string-length (m 1)))
                                                  (m 2)))]
                   [(#/varn{(.*)}/ x) => (lambda (m) (format "~an" (m 1)))]
                   [else x]))
                (remp (cut string=? <> "") (string-split args-string #\space))))
      '()))

(define (file->prot* file)
  (let ([line* (file->list file)])
     (map
      (lambda (x)
        (list (string->symbol (x 1))
              (adjust-args (x 2))))
      (remq #f (append (map #/proto{([^}]+)}{(.*)}{procedure}/ line*)
                       (map #/proto{([^}]+)}{(.*)}{.+exprtype}/ line*))

     ))))

(for-each
 (lambda (file)
   (format (current-error-port) "~a\n" (first file))
   (with-output-to-file (string-append out-dir (first file) ".txt")
     (lambda ()
   (let ([proto* (file->prot* (string-append r6rs-dir (first file)))])
     (format #t "Title: ~a\n\n~a\n\n" (third file) (fifth file))
     (if (fourth file) ;; library-name
         (for-each (lambda (x) (format #t "library: ~a\n\n" x)) (fourth file))
         (format (current-error-port) "~a : library name not specified\n" (first file)))
     (for-each
      (lambda (prot)
        (format (current-error-port) "[[~a]]\n" prot)
        (format #t
"Function: ~a

See <~a>
HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
Prototype:
> ~a\n\n\n"
        (car prot)
        (hashtable-ref symbols (car prot) #f)
        `(,(car prot) ,@(cadr prot)))
        )
      proto*)
     ))))
 files)
;; (import (rnrs)
;;         (mosh))

;; (define MAX 100000)

;; (with-output-to-file "/tmp/mosh.txt"
;;   (lambda ()
;;     (let loop ([i 0])
;;       (when (zero? (mod i 20))
;;         (newline))
;;       (cond
;;        [(= i MAX) 'done]
;;        [else
;;         (format #t "~a," i)
;;         (loop (+ i 1))]))))
