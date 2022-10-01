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
                 rewrite-export path-dirname);; todo clean this up!
         (import (rnrs)
                 (except (mosh) available-features available-libraries)
                 (match))

;; R7RS Available features and libraries.
;; TODO(higepon): Use available-features in (mosh) library once 0.2.8rc5 is out.
(define (available-features)
  '(r6rs r7rs mosh))

(define (available-libraries)
    `((srfi-0) (srfi-1) (mosh) (rnrs arithmetic bitwise)))

;; The main API.
;; Returns (expanded-library-form and included-file).
;; We track included-file for library cache.
(define (rewrite-define-library dirname exp)
  (match exp
    [('define-library (name* ...) lib-decl* ...)
      (let-values (((lib-decl* export* import* included-file*) (rewrite-lib-decl* dirname lib-decl*)))
        (values `(library ,name* (export ,@export*) (import ,@import*) ,@lib-decl*)
                included-file*))]
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
(define rewrite-lib-decl*
  (case-lambda
    [(dirname lib-decl*)
      (rewrite-lib-decl* dirname lib-decl* '() '() '())]
    [(dirname lib-decl* export* import* included-file*)
        (let loop ([ret '()]
                   [decl* lib-decl*]
                   [export* export*]
                   [import* import*]
                   [included-file* included-file*])
            ;(format #t "decl=~a export=~a import=~a ret=~a\n" decl* export* import* ret)
            (if (null? decl*)
                (values ret (map rewrite-export export*) import* included-file*)
                (match (car decl*)
                    ;; (import 〈import spec〉 . . . )
                    ;; In R7RS import can appears multiple times but not in R6RS. We have to merge them into one.
                    [('import spec* ...)
                      (loop ret (cdr decl*) export* (append import* spec*) included-file*)]
                    ;; (export 〈export spec〉 . . . )
                    [('export spec* ...)
                      (loop ret (cdr decl*) (append export* spec*) import* included-file*)]
                    ;; (include 〈filename1〉〈filename2〉 . . . )
                    [('include path* ...)
                      ;; Expand it to multiple include and pass it to psyntax later.
                      ;; Note we append dirname to path so that (include "foo.scm") works.
                      (let ([include-path* (map (lambda (path) (string-append dirname path)) path*)]
                            [new-decl* (map (lambda (path) `(include ,(string-append dirname path))) path*)])
                        (loop (append ret new-decl*) (cdr decl*) export* import* (append included-file* include-path*)))]
                    ;; (include 〈filename1〉〈filename2〉 . . . )
                    [('include-ci path* ...)
                    ;; Expand it to multiple include-ci and pass it to psyntax later.
                    ;; Note we append dirname to path so that (include-ci "foo.scm") works.
                    (let* ([include-path* (map (lambda (path) (string-append dirname path)) path*)]
                           [new-decl* (map (lambda (path) `(include-ci ,(string-append dirname path))) path*)])
                        (loop (append ret new-decl*) (cdr decl*) export* import* (append included-file* include-path*)))]
                    ;; (include-library-declarations 〈filename〉)
                    [('include-library-declarations path)
                      (let* ([include-path (string-append dirname path)]
                             [new-decl* (file->sexp-list include-path)])
                        (let-values (((new-decl2* new-export* new-import* new-included*) (rewrite-lib-decl* dirname new-decl* export* import* (append included-file* (list include-path )))))
                          ;; We call rewrite-lib-decl* because expanded include may have something we care about.
                          (loop (append ret new-decl2*)
                                (cdr decl*) new-export* new-import* new-included*)))]
                    ;; (include-library-declarations 〈filename1〉〈filename2〉 . . . )
                    [('include-library-declarations path* ...)
                      (let ([new-decl* (map (lambda (path) `(include-library-declarations ,path)) path*)])
                        (let-values (((new-decl2* new-export* new-import* new-included*) (rewrite-lib-decl* dirname new-decl* export* import* included-file*)))
                          (loop (append ret new-decl2*)
                                (cdr decl*) new-export* new-import* new-included*)))]
                    [('cond-expand clause* ...)
                      (let ([new-decl* (rewrite-cond-expand (car decl*))])
                        (let-values (((new-decl2* new-export* new-import* new-included*) (rewrite-lib-decl* dirname new-decl* export* import* included-file*)))
                          (loop (append ret new-decl2*)
                                (cdr decl*) new-export* new-import* new-included*)))]
                    [any
                      (loop (append ret `(,any)) (cdr decl*) export* import* included-file*)])))]))

;; Returns list of〈library declaration〉.
(define (rewrite-cond-expand expr)
   ;;(format #t "expr=~a\n" expr)
   (match expr
     [(_)
       (assertion-violation 'cond-expand "Unfulfilled cond-expand" expr)]
     [(_ ('else lib-decl* ...))
       lib-decl*]
     [(_ (('and) lib-decl* ...) more-clause* ...)
       lib-decl*]
     [(_ (('and feature1 feature2 ...) lib-decl* ...) more-clause* ...)
       `((cond-expand
           (,feature1
             (cond-expand
               ((and ,@feature2) ,@lib-decl*) ,@more-clause*))
            ,@more-clause*))]
     [(_ (('or) lib-decl* ...) more-clause* ...)
       `((cond-expand ,@more-clause*))]
     [(_ (('or feature1 feature2 ...) lib-decl* ...) more-clause* ...)
       `((cond-expand
           (,feature1 ,@lib-decl*)
           (else
             (cond-expand
               ((or ,@feature2) ,@lib-decl*) ,@more-clause*))))]
     [(_ (('not feature) lib-decl* ...) more-clause* ...)
       `((cond-expand (,feature
                       (cond-expand ,@more-clause*))
                     (else ,@lib-decl*)))]
     [(_ ((? symbol? feature) lib-decl* ...) more-clause* ...)
        ;; This feature is available.
        (if (member feature (available-features))
            lib-decl*
            `((cond-expand ,@more-clause*)))]
     [(_ (('library name* ...) lib-decl* ...) more-clause* ...)
        ;; This library is available.
        (if (member name* (available-libraries))
            lib-decl*
            `((cond-expand ,@more-clause*)))]
        ))

(define (rewrite-export exp)
   (match exp
     [('rename from to)
        `(rename (,from ,to))]
     [name name]))

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