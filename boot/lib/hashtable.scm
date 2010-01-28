; hashtable.scm - hashtable procedures
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
;  $Id: hashtable.scm 621 2008-11-09 06:22:47Z higepon $

;; Ignore k
;; (define (make-eqv-hashtable . k)
;;   (make-hashtable (lambda (x)
;;                     (if (number? x)
;;                         (string-hash (number->string x))
;;                         x)) eqv?))

(define (hashtable-update! hashtable key proc default)
  (cond
   [(hashtable-mutable? hashtable)
    (hashtable-set!
     hashtable key
     (proc (hashtable-ref
            hashtable key default)))]
   [else
    (assertion-violation 'hashtable-update! "can't update! immutable hashtable")]))

(define (hashtable-entries hashtable)
  (let* ([keys (hashtable-keys hashtable)]
         [vals (make-vector (vector-length keys))])
    (let loop ([i 0])
      (cond
       [(>= i (vector-length keys))
        (values keys vals)]
       [else
        (vector-set! vals i (hashtable-ref hashtable (vector-ref keys i)))
        (loop (+ i 1))]))))

(define (hashtable-for-each proc ht)
  (let1 keys (hashtable-keys ht)
    (vector-for-each
     (lambda (key)
       (proc key (hashtable-ref ht key)))
     keys)))


(define (hashtable-map proc ht)
  (let1 keys (vector->list (hashtable-keys ht))
    ($map1
     (lambda (key)
       (proc key (hashtable-ref ht key)))
     keys)))

(define (hashtable-keys->list ht)
  (vector->list (hashtable-keys ht)))

(define (hashtable->alist ht)
  (hashtable-map cons ht))

(define (alist->eq-hash-table alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (cdr x)))
              alist)
    hashtable))

(define (ralist->eq-hash-table alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (cdr x) (car x)))
              alist)
    hashtable))
