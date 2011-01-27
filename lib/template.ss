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
  (export template-vars template->sexp eval-template eval-template-file ref)
  (import (rnrs)
          (shorten)
          (mosh)
          (mosh file)
          (srfi :39)
          (rnrs eval)
          (mosh control)
          (irregex))

(define template-vars (make-parameter #f))

(define-syntax ref
  (lambda (x)
    (syntax-case x ()
      [(_ alist key)
       #'(assoc-ref alist 'key)])))

(define (eval-template-file file vars)
  (eval-template (file->string file) vars))

(define (eval-template template vars)
  (parameterize ([template-vars vars])
    (let1 templ (template->sexp template vars)
      (if (eof-object? templ)
          '()
          (eval templ (environment '(rnrs) '(mosh) '(template)))))))

;; http://d.hatena.ne.jp/yuum3/20080203/1202049898
(define (compile-elem templ port)
  (cond [(string=? templ "") #t]
        [(irregex-search (string->irregex "^<%=(.+?)%>(.*)" 's) templ) =>
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
    (unless (null? variable*)
      (display "(let () " port))
    (for-each
     (^(variable)
       (format port "(define ~a ~s)" (car variable) (cdr variable))
       )
     variable*)
    (compile-elem template port)
    (unless (null? variable*)
      (display ") " port))
    (read (open-string-input-port (get-string)))))
)

