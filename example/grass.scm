;   grass.scm - Grass interpreter
;   http://www.blue.sky.or.jp/grass/
;
;   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;  $Id$
;
;  Update: 2008/08/01 Bug fixed by IRIE Shinsuke.


(define (make-app n m)
  `(app ,n ,m))

(define (make-abs n c)
  `(abs ,n ,c))

(define (make-primitive tag val proc)
  `(primitive ,tag ,val ,proc))

(define (primitive? p)
  (match p
    [('primitive . more) #t]
    [else                #f]))

(define (primitive-val p) (third p))
(define (primitive-proc p) (fourth p))
(define (primitive-is? p tag) (eq? (second p) tag))

(define church-true  `((,(make-abs 1 `(,(make-app 3 2)))) . ((() . ()))))
(define church-false `((,(make-abs 1 '())) . ()))

(define (make-char ch)
  (make-primitive 'char ch (lambda (c) (unless (primitive-is? c 'char)
                                               (error "char:character required"))
                                   (if (eq? (primitive-val c) ch)
                                       church-true
                                       church-false))))

(define out (make-primitive 'proc '() (lambda (c) (unless (primitive-is? c 'char)
                                            (error "out:character required"))
                                      (display (primitive-val c))
                                      c)))

(define succ (make-primitive 'proc '() (lambda (c) (unless (primitive-is? c 'char)
                                                     (error "succ:character required"))
                                        (let1 v (char->integer (primitive-val c))
                                          (if (= v 255)
                                              (make-char (integer->char 0))
                                              (make-char (integer->char (+ v 1))))))))

(define in (make-primitive 'proc '() (lambda (x) (let1 c (read-char)
                                          (if (eof-object? c)
                                              x
                                              (make-char c))))))

(define w (make-char #\w))

(define (grass-eval text env dump)
  (define (rec code env dump)
    (define (env-ref i)  (list-ref env (- i 1)))
    (define (nth-code n) (car (env-ref n)))
    (define (nth-env n)  (cdr (env-ref n)))
    (match code
      [(('app m n) . c)
       (let1 mth (env-ref m)
         (if (primitive? mth)
             (rec c
                  `(,((primitive-proc mth) (env-ref n)) . ,env)
                  dump)
             (rec (nth-code m)
                  `((,(nth-code n) . ,(nth-env n)) . ,(nth-env m))
                  `((,c . ,env) . ,dump))))]
      [(('abs 1 cc) . c)
       (rec c `((,cc . ,env) . ,env) dump)]
      [(('abs n cc) . c)
       (rec c `((,(list (make-abs (- n 1) cc)) . ,env) . ,env) dump)]
      [()
       (if (null? dump)
           '()
           (rec (caar dump)
                       `(, (car env) . , (cdar dump))
                       (cdr dump)))]
      [else
       (error "grass-eval runtime error")]))
  (rec (parse text) env dump))

(define e0 `(,out ,succ ,w ,in))
(define d0 `(((,(make-app 1 1)) . ()) (() . ())))

(define (parse text)
  (define (normalize t)
   (regexp-replace-all #/wvW/
    (list->string
     (filter (lambda (p) (memq p '(#\w #\W #\v)))
            (memq #\w
             (string->list
              (regexp-replace-all #/Ｗ/ (regexp-replace-all #/ｖ/ (regexp-replace-all #/ｗ/ t "w") "v") "W")))))
    "wW"))
  (define (parse-body t)
    (aif (#/^(W+)(w+)(.*)/ (if t t ""))
         (cons (make-app (string-length (it 1)) (string-length (it 2)))
               (parse-body (it 3)))
         '()))
  (map (lambda (x)
         (aif (#/^(w+)(.*)/ x)
              (make-abs (string-length (it 1)) (parse-body (it 2)))
              (error "syntax error")))
       (string-split (normalize text) #\v)))

(grass-eval "ｗｗＷＷｗv
             ｗｗｗｗＷＷＷｗｗＷｗｗＷＷＷＷＷＷｗｗｗｗＷｗｗv
             ｗＷＷｗｗｗＷｗｗｗｗＷｗｗｗｗｗｗＷｗｗｗｗｗｗｗｗｗ" e0 d0)


(grass-eval "ｗＷＷｗｗｗｗ" e0 d0)

(grass-eval "wWWWwWWWWwv wWWwWWWwv wWWwWWWwv wWWwWWWwv wWWwWWWwv wWWwWWWwv
wWWwWWWwv wWWWwWWWWwv wWWwwwwwwwwwwwwWWWWWWwWWWWWWwWWWWWWWWWwWWWWWWWWW
WWWWWWwWWWWWWWWWWWWWwWWWWWWWWWWWWWWWwwWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWww
wwwWWWWWWWWWWWWWWWWwwwwwwwWWWWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWWWWWWwWWWW
WWWWWWWWWWWWWWWWWWWwwWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwWWWWWWWWWWWWW
WWWWWWWWwWWWWWWWWWWWWWWWWWWWWWwwWWWWWWWWWWWWWWWWWWWWWWWWwWWWWWWWWWWWWW
WWWWWWWWWWWWWwwwwwwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWW
WWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWwWWWWWWWWWWW
WWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWWWWWW
WWWWWWWWWWWWWwwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwww
wwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwwwWWWWWWW
WWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWW
WWWWWWWWWWWWWWwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwww
wwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwwwwwww
wwwwwwwwwwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwww
wwwwwwwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwww
wwwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwwwwwww
wwwWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwwwwwWWW
WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwwwwwwww" e0 d0)


;(grass-eval "無限に草植えときますねｗＷＷｗｗｗｗＷＷｗｗ" e0 d0)
