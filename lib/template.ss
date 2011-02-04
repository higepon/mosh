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
          (mosh control)
          (irregex))

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
  (cond [(string=? templ "") #t]
        [(irregex-search (string->irregex "^<%include (.+?)\\s*%>(.*)" 's) templ) =>
         (^m
          (let1 path (if (template-dir) (string-append (template-dir) "/" (irregex-match-substring m 1)) (irregex-match-substring m 1))
            (compile-elem (string-append (file->string path) (irregex-match-substring m 2)) port)))]
        ;; comment
        [(irregex-search (string->irregex "^<%#(.+?)%>(.*)" 's) templ) =>
         (^m
          (compile-elem (irregex-match-substring m 2) port))]
        ;; output with escape
        [(irregex-search (string->irregex "^<%=(.+?)%>(.*)" 's) templ) =>
         (^m
          (format port "(display (h ~a))" (irregex-match-substring m 1))
          (compile-elem (irregex-match-substring m 2) port))]
        [(irregex-search (string->irregex "^<%=unsafe(.+?)%>(.*)" 's) templ) =>
         (^m
          (format port "(display ~a)" (irregex-match-substring m 1))
          (compile-elem (irregex-match-substring m 2) port))]
        [(irregex-search (string->irregex "^<%(.+?)%>(.*)" 's) templ) =>
         (^m
          (format port "~a" (irregex-match-substring m 1))
          (compile-elem (irregex-match-substring m 2) port))]
        [(irregex-search (string->irregex "^(.+?)<%(.*)" 's) templ) =>
         (^m
          (format port "(display ~s)" (irregex-match-substring m 1))
          (compile-elem (string-append "<%" (irregex-match-substring m 2)) port))]
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

