; queue.ss
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;  $Id: concurrent.ss 621 2008-11-09 06:22:47Z higepon $

(library (mosh queue)
  (export make-queue queue-empty? queue-push! queue-pop! queue-append!)
  (import (only (rnrs) define cons quote null? cond list car cdr cddr when caar else let error cdar)
          (only (rnrs mutable-pairs) set-car! set-cdr!))

(define (make-queue) (cons '() '()))

(define (queue-empty? queue)
  (null? (cdr queue)))

(define (queue-push! queue obj)
  (cond
   [(queue-empty? queue)
    (set-car! queue (list obj))
    (set-cdr! queue (car queue))]
   [else
    (set-cdr! (cdr queue) (list obj))
    (set-cdr! queue (cddr queue))]))

;; todo
;; this is a naive implementation
(define (queue-append! lhs rhs)
  (let loop ([empty? (queue-empty? rhs)])
    (cond
     [empty? lhs]
     [else
      (queue-push! lhs (queue-pop! rhs))
      (loop (queue-empty? rhs))])))

(define (queue-pop! queue)
  (when (queue-empty? queue)
    (error 'queue-pop! "queue is empty"))
  (let ([val (caar queue)])
    (set-car! queue (cdar queue))
    (when (null? (car queue))
      (set-cdr! queue '()))
    val))
)
