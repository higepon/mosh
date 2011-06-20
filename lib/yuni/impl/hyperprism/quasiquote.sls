(library (yuni scheme synrule quasiquote)
         (export
           quasiquote
           unquote
           unquote-splicing)
         (import 
           (yuni util invalid-form)
           (except (rnrs) quasiquote unquote unquote-splicing))

;MOSH: ERROR

(define (ERROR . e)
  (error "err from alexpander" e))

(define-invalid-forms unquote unquote-splicing)

;; alexpander.scm: a macro expander for scheme.
;; $Id: alexpander.scm,v 1.65 2007/11/05 02:50:34 al Exp $

;; Copyright 2002-2004,2006,2007 Al Petrofsky <alexpander@petrofsky.org>

;; LICENSING (3-clause BSD or GNU GPL 2 and up)

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;   Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;; 
;;   Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.
;; 
;;   Neither the name of the author nor the names of its contributors
;;     may be used to endorse or promote products derived from this
;;     software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; Alternatively, you may redistribute, use, or modify this software
;; according to the terms of the GNU General Public License as
;; published by the Free Software Foundation (fsf.org); either version
;; 2, or (at your option) any later version.

;; INTRODUCTION:

;; This file implements a macro-expander for r5rs scheme (plus some
;; interesting extensions).  There is no magic here to hook this into
;; your native eval system: this is a simple data-in, data-out program
;; that takes a macro-using program represented as scheme data and
;; produces an equivalent macro-free program represented as scheme
;; data.

;; This is mostly intended as a demonstration.  Although it certainly
;; could be useful for adding macros to a simple scheme system that
;; lacks any macros, it may not be feasible to get it to interact
;; properly with a low-level macro system or a module system.

;; The expander is written in portable r5rs scheme, except for one use
;; of the pretty-print procedure which you can easily comment out.

;; To try it out, just load the file and execute (alexpander-repl).
;; Skip to the "BASIC USAGE" section for more information.

;; To find the latest version of this program, try here:
;;    http://petrofsky.org/src/alexpander.scm
;;
;; To find older versions or the log messages between versions, try here:
;;    http://petrofsky.org/src/RCS/alexpander.scm,v

;; If you are wondering what "r5rs scheme" is, see: 
;;    Richard Kelsey, William Clinger, and Jonathan Rees, "Revised^5
;;    report on the algorithmic language Scheme", Higher-Order and
;;    Symbolic Computation, 11(1):7-105, 1998.  Available at:
;;       PDF: http://www-swiss.ai.mit.edu/~jaffer/r5rs.pdf
;;       LaTeX source: ftp://swiss.csail.mit.edu/pub/scheme-reports/r5rs.tar.gz

;; -- snip -- ;; HYP

;; Improved nested unquote-splicing.  

;; Quasiquote is extended to make commas and comma-ats distributive
;; over a nested comma-at, as in Common Lisp's backquote.  See my
;; 2004-09-03 usenet article <87pt53f9f2.fsf@radish.petrofsky.org>,
;; Bawden's 1999 quasiquotation paper, and Appendix C of Steele's
;; "Common Lisp the Language 2nd edition".

;;   <splicing unquotation 1> ---> ,@<qq template 0>
;;                               | (unquote-splicing <qq template 0>)
;;
;;   <splicing unquotation D> ---> ,@<qq template D-1>
;;                               | ,<splicing unquotaion D-1>
;;                               | ,@<splicing unquotaion D-1>
;;                               | (unquote-splicing <qq template D-1>)
;;                               | (unquote <splicing unquotaion D-1>)
;;                               | (unquote-splicing <splicing unquotaion D-1>)

;; When a comma at-sign and the expression that follows it are being
;; replaced by the elements of the list that resulted from the
;; expression's evaluation, any sequence of commas and comma at-signs
;; that immediately preceded the comma at-sign is also removed and is
;; added to the front of each of the replacements.

;;  (let ((x '(a b c))) ``(,,x ,@,x ,,@x ,@,@x))
;;  => `(,(a b c) ,@(a b c) ,a ,b ,c ,@a ,@b ,@c)
;;
;;  ``(,,@'() ,@,@(list))
;;  => `()
;;
;;  `````(a ,(b c ,@,,@,@(list 'a 'b 'c)))
;;  => ````(a ,(b c ,@,,@a ,@,,@b ,@,,@c))
;;  
;; (let ((vars '(x y)))
;;   (eval `(let ((x '(1 2)) (y '(3 4)))
;;            `(foo ,@,@vars))
;;         (null-environment 5)))
;; => (foo 1 2 3 4)
;; Quasiquote uses let-syntax scope so that it can recognize
;; nested uses of itself using a syntax-rules literal (that
;; is, the quasiquote binding that is visible in the
;; environment of the quasiquote transformer must be the same
;; binding that is visible where quasiquote is used).
(define-syntax qq
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ ,x        ())      (do-next x))
    ((_ (,@x . y) ())      (qq y () make-splice x))
    ((_ `x         depth)  (qq x (depth) make-list 'quasiquote))
    ((_ ,x        (depth)) (qq x  depth  make-list 'unquote))
    ((_ (,x  . y) (depth)) (qq-nested-unquote (,x  . y) (depth)))
    ((_ (,@x . y) (depth)) (qq-nested-unquote (,@x . y) (depth)))
    ((_ ,@x        depth)  (unquote-splicing-ERROR ,@x))
    ((_ (x . y)    depth)  (qq x depth qq-cdr y depth make-pair))
    ((_ #(x y ...) depth)  (qq (x) depth qq-cdr #(y ...) depth
                               make-vector-splice))
    ((_ x          depth)  (do-next 'x))))

(define-syntax do-next
  (syntax-rules ()
    ((_ expr original-template) expr)
    ((_ expr next-macro . tail) (next-macro expr . tail))))

(define-syntax unquote-splicing-ERROR
  (syntax-rules ()
    ((_ ,@x stack ... original-template)
     (unquote-splicing-ERROR (,@x in original-template)))))

(define-syntax qq-cdr
  (syntax-rules ()
    ((_ car cdr depth combiner) (qq cdr depth combiner car))))

(define-syntax qq-nested-unquote
  (syntax-rules ()
    ((_ ((sym x) . y) (depth))
     (qq (x) depth make-map sym qq-cdr y (depth) make-splice))))

(define-syntax make-map
  (syntax-rules (quote list map lambda)
    ((_ '(x) sym) (do-next '((sym x))))
    ((_ (list x) sym) (do-next (list (list 'sym x))))
    ((_ (map (lambda (x) y) z) sym)
     (do-next (map (lambda (x) (list 'sym y)) z)))
    ((_ expr sym)
     (do-next (map (lambda (x) (list 'sym x)) expr)))))

(define-syntax make-pair
  (syntax-rules (quote list)
    ((_ 'y 'x) (do-next '(x . y)))
    ((_ '() x) (do-next (list x)))
    ((_ (list . elts) x) (do-next (list x . elts)))
    ((_ y x) (do-next (cons x y)))))

(define-syntax make-list
  (syntax-rules (quote)
    ((_ y x) (make-pair '() y make-pair x))))

(define-syntax make-splice
  (syntax-rules ()
    ((_ '() x) (do-next x))
    ((_ y x) (do-next (append x y)))))

(define-syntax make-vector-splice
  (syntax-rules (quote list vector list->vector)
    ((_ '#(y ...) '(x))     (do-next '#(x y ...)))
    ((_ '#(y ...) (list x)) (do-next (vector x 'y ...)))
    ((_ '#()      x)        (do-next (list->vector x)))
    ((_ '#(y ...) x)        (do-next (list->vector
                                       (append x '(y ...)))))
    ((_ y '(x))             (make-vector-splice y (list 'x)))
    ((_ (vector y ...) (list x)) (do-next (vector x y ...)))
    ((_ (vector y ...) x)   (do-next (list->vector
                                       (append x (list y ...)))))
    ((_ (list->vector y) (list x)) (do-next (list->vector
                                              (cons x y))))
    ((_ (list->vector y) x) (do-next (list->vector
                                       (append x y))))))
(define-syntax quasiquote
    (syntax-rules ()
    ((_ template) (let () (qq template () template)))))

)
