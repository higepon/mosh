; template.ss - Simple template
;
;   Copyright (c) 2011  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;
 (library (template)
  (export template-vars template->sexp eval-template eval-template-file ref h template-dir)
  (import (rnrs)
          (shorten)
          (mosh)
          (mosh file)
          (srfi :39)
          (rnrs eval)
          (rename (mosh cgi) (escape h))
          (mosh control))

(define template-vars (make-parameter #f))
(define template-dir (make-parameter #f))

(define-syntax ref
  (lambda (x)
    (syntax-case x ()
      [(_ alist key)
       #'(assoc-ref alist 'key)])))

(define (eval-template-file file vars . import-spec*)
  (let1 path (if (template-dir) (string-append (template-dir) "/" file) file)
    (apply eval-template (file->string path) vars import-spec*)))

(define (eval-template template vars . import-spec*)
  (parameterize ([template-vars vars])
    (let1 templ (template->sexp template vars)
      (if (eof-object? templ)
          '()
          (eval templ (apply environment '(rnrs) '(mosh) '(template) '(match) import-spec*))))))

;; http://d.hatena.ne.jp/yuum3/20080203/1202049898
(define (compile-elem templ port)
;  (format (current-error-port) "\n\n~s\n\n" templ)
  (cond [(or (not templ) (string=? templ "")) #t]
        [((string->regexp "^<%include ([^%]+?)\\s*%>((.|\n)*)" 's) templ) =>
         (^m
          (let1 path (if (template-dir) (string-append (template-dir) "/" (m 1)) (m 1))
            (compile-elem (string-append (file->string path) (if (m 2) (m 2) "")) port)))]
        ;; comment
        [((string->regexp "^<%#(.+?)%>((.|\n)*)" 's) templ) =>
         (^m
          (compile-elem (m 2) port))]
        [((string->regexp "^<%=unsafe(.+?)%>((.|\n)*)" 's) templ) =>
         (^m
          (format port "(display ~a)" (m 1))
          (compile-elem (m 2) port))]
        ;; output with escape
        [((string->regexp "^<%=([^%]+?)%>((.|\n)*)" 's) templ) =>
         (^m
          (format port "(display (h ~a))" (m 1))
          (compile-elem (m 2) port))]
        [((string->regexp "^<%((.|\n)+?)%>((.|\n)*)" 's) templ) =>
         (^m
 ;         (format (current-error-port) "hoge=<~s><~s>\n" (m 1) (m 3))
          (format port "~a" (m 1))
          (compile-elem (m 3) port))]
        [((string->regexp "^(([^%])*)<%((.|\n)*)" 's) templ) =>
         (^m
          (format port "(display ~s)" (m 1))
          (compile-elem (string-append "<%" (if (m 3) (m 3) "")) port))]
        [else
         (format port "(display ~s)" templ)]))

(define (template->sexp template variable*)
  (let-values (([port get-string] (open-string-output-port)))
    (compile-elem template port)
    (let1 body (let1 p (open-string-input-port (get-string))
                 (let loop ([sexp (read p)]
                            [ret '()])
                   (if (eof-object? sexp)
                       (reverse ret)
                       (loop (read p) (cons sexp ret)))))
      (if (null? body)
          #f
          `(let (,@(map (^v `(,(car v) ,(cdr v))) variable*))
             ,@body)))))
)

