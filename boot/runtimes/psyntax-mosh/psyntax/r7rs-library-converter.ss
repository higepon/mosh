; Converter from R7RS library to R6RS library.
;
;   Copyright (c) 2022 Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; N.B. For testablity. This library should not depend on other psyntax files.
(library (psyntax r7rs-library-converter)
         (export rewrite-define-library rewrite-lib-decl*
                 rewrite-export rewrite-body parse-define-library path-dirname);; todo clean this up!
         (import (rnrs)
                 (mosh)
                 (match))

;; The main API.
(define (rewrite-define-library dirname exp)
  (match exp
    [('define-library (name* ...) lib-decl*)
      `(library ,name* ,(rewrite-lib-decl* dirname lib-decl*))]
    [else
      (assertion-violation 'rewrite-define-library "malformed library" `(,exp))]))

;; Rewrite list of 〈library declaration〉and return list of 〈library declaration〉.
;;  〈library declaration〉 is any of:
;;     (export 〈export spec〉 . . . )
;;     (import 〈import set〉 . . . )
;;     (begin 〈command or definition〉 . . . )
;;     (include 〈filename1〉 〈filename2〉 . . . )
;;     (include-ci 〈filename1〉 〈filename2〉 . . . )
;;     (include-library-declarations 〈filename1〉〈filename2〉 . . . )
;;     (cond-expand 〈ce-clause1〉 〈ce-clause2〉 . . . )    
(define (rewrite-lib-decl* dirname lib-decl*)
  (let loop ([ret '()]
             [decl* lib-decl*])
    (if (null? decl*)
        ret
        (match (car decl*)
            ;; (export 〈export spec〉 . . . )
            [('export spec* ...)
              (loop (append ret `((export ,@(rewrite-export spec*)))) (cdr decl*))]
            ;; (include 〈filename1〉〈filename2〉 . . . )                    
            [('include path* ...)
              ;; Expand it to multiple include and pass it to psyntax later.
              ;; Note we append dirname to path so that (include "foo.scm") works.
              (let ([new-decl* (map (lambda (path) `(include ,(string-append dirname path))) path*)])
                (loop (append ret new-decl*)
                      (cdr decl*)))]            
            ;; (include-library-declarations 〈filename〉)
            [('include-library-declarations path)
              (let ([new-decl* (file->sexp-list (string-append dirname path))])
                ;; We call rewrite-lib-decl* because expanded include may have something we care about.
                (loop (append ret (rewrite-lib-decl* dirname new-decl*))
                      (cdr decl*)))]
            ;; (include-library-declarations 〈filename1〉〈filename2〉 . . . )                    
            [('include-library-declarations path* ...)
              (let ([new-decl* (map (lambda (path) `(include-library-declarations ,path)) path*)])
                (loop (append ret (rewrite-lib-decl* dirname new-decl*))
                (cdr decl*)))]
            [any
              (loop (append ret `(any))
                    (cdr decl*))]))))

            

(define (parse-define-library exp)
  (match exp
    [('define-library (name* ...)
                      ('export export* ...)
                      ('import import* ...)
       body* ...)
        (values name* export* import* body*)]
    [else (assertion-violation 'parse-define-library "malformed library" `(,exp))]))

(define (rewrite-export exp)
   (match exp
     [(('rename from to) other ...)
        `((rename (,from ,to)) ,@(rewrite-export other))]
     [(one other ...)
        `(,one ,@(rewrite-export other))]
     [() '()]))

(define (rewrite-include-lds dirname path)
    (file->sexp-list (string-append dirname path)))

(define (rewrite-body dirname exp)
  (flatten
     (map
       (lambda (e)
         (match e
           [('include path* ...)
             (map (lambda (path) `(include ,(string-append dirname path))) path*)]
           [('include-library-declarations path* ...) 
            (flatten (map (lambda (path) (rewrite-include-lds dirname path)) path*))]
           [else `(,e)])) exp)))

;; Utilities.
(define (file->sexp-list file)
  (with-input-from-file file
    (lambda ()
      (let loop ([line (read)]
                 [ret '()])
        (cond
         [(eof-object? line) (reverse ret)]
         [else
          (loop (read) (cons line ret))])))))

(define (fold1 kons knil lst)
    (if (null? lst)
        knil
       (fold1 kons (kons (car lst) knil) (cdr lst))))


(define (flatten lists)
    (fold1 (lambda (right left)
                    (append left right))
                '() lists))

;; from pathutils-nmosh.ss
(define (path-dirname pth)
  (car (split-dir+base pth)))

(define (split-dir+base pth)
  (define (itr cur rest)
    (if (pair? rest)
      (if (char=? (car rest) #\/)
        (cons
          (list->string (reverse rest))
          (list->string cur)) ;basename
        (itr (cons (car rest) cur) (cdr rest)))
      (cons "" pth)))
  (let ((p (pathfilter pth)))
    (itr '() (reverse  (string->list p)))))

(define (run-win32-np?) (string=? "win32" (host-os)))

(define pathfilter
  (if (run-win32-np?)
    (lambda (str)
      (and (string? str)
	   (list->string (map (lambda (e) (if (char=? e #\\) #\/ e)) (string->list str)))))
    (lambda (str) str)))

)