; enum.scm - enum
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

;; Originally from Ypsilon Scheme

(define enum-set-rtd (make-record-type-descriptor 'enum-set #f #f #f #f '#((mutable type) (mutable members))))
(define enum-set-rcd (make-record-constructor-descriptor enum-set-rtd #f #f))
(define make-enum-set (record-constructor enum-set-rcd))
(define enum-set-members (record-accessor enum-set-rtd 1)) (define enum-set-type (record-accessor enum-set-rtd 0))

(define enum-type-rtd (make-record-type-descriptor 'enum-type #f #f #f #f '#((mutable universe) (mutable indexer))))
(define enum-type-rcd (make-record-constructor-descriptor enum-type-rtd #f #f))
(define make-enum-type (record-constructor enum-type-rcd))
(define enum-type-indexer (record-accessor enum-type-rtd 1))
(define enum-type-universe (record-accessor enum-type-rtd 0))

(define (make-enumeration-type symbol-list)
  (let ([ht (make-eq-hashtable)])
    (let loop ([symbol-list symbol-list]
               [i 0])
      (if (null? symbol-list)
          '()
          (begin (hashtable-set! ht (car symbol-list) i)
                 (loop (cdr symbol-list) (+ i 1)))))
    (make-enum-type symbol-list
                    (lambda (symbol)
                      (hashtable-ref ht symbol #f)))))


(define (make-enumeration symbol-list)
  (cond
   [(and (list? symbol-list) (for-all symbol? symbol-list))
    (make-enum-set (make-enumeration-type symbol-list) symbol-list)]
   [else
    (assertion-violation 'make-enumeration "argument 1 must be a list of symbols")]))


(define (enum-set-universe enum-set)
  (make-enum-set (enum-set-type enum-set)
                 (enum-type-universe (enum-set-type enum-set))))

(define (enum-set-indexer enum-set)
  (enum-type-indexer (enum-set-type enum-set)))

(define (enum-set-constructor enum-set)
  (lambda (symbol-list)
    (let ([universe (enum-type-universe (enum-set-type enum-set))])
      (if (for-all (lambda (x) (memq x universe)) symbol-list)
          (make-enum-set (enum-set-type enum-set) symbol-list)
          (assertion-violation 'enum-set-constructor "the symbol list must all belong to the universe." universe symbol-list)))))

(define (enum-set->list enum-set)
  (let ([universe (enum-type-universe (enum-set-type enum-set))]
        [members (enum-set-members enum-set)])
    (let loop ([universe universe])
      (cond
       [(null? universe) '()]
       [(memq (car universe) members)
        (cons (car universe) (loop (cdr universe)))]
       [else
        (loop (cdr universe))]))))

(define (enum-set-member? symbol enum-set)
  (and (memq symbol (enum-set-members enum-set)) #t))

(define (enum-set-subset? enum-set1 enum-set2)
  (and
   (let ([enum-set2-univese (enum-set->list (enum-set-universe enum-set2))])
     (for-all
      (lambda (symbol) (memq symbol enum-set2-univese))
      (enum-set->list (enum-set-universe enum-set1))))
   (for-all
    (lambda (symbol) (enum-set-member? symbol enum-set2))
    (enum-set-members enum-set1))))

(define (enum-set=? enum-set1 enum-set2)
  (and (enum-set-subset? enum-set1 enum-set2)
       (enum-set-subset? enum-set2 enum-set1)))

(define (enum-set-union enum-set1 enum-set2)
  (define (union lst1 lst2)
    (let loop ([ret lst1]
               [lst lst2])
      (cond
       [(null? lst) ret]
       [(memq (car lst) ret)
        (loop ret (cdr lst))]
       [else
        (loop (cons (car lst) ret) (cdr lst))])))
  (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
      (make-enum-set (enum-set-type enum-set1)
                     (union (enum-set-members enum-set1) (enum-set-members enum-set2)))
      (assertion-violation 'enum-set-union "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

(define (enum-set-intersection enum-set1 enum-set2)
  (define (intersection lst1 lst2)
    (let loop ([ret '()]
               [lst lst1])
      (if (null? lst)
          ret
          (cond
           [(memq (car lst) lst2)
             (loop (cons (car lst) ret) (cdr lst))]
           [else
            (loop ret (cdr lst))]))))
  (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
      (make-enum-set (enum-set-type enum-set1)
                     (intersection (enum-set-members enum-set1) (enum-set-members enum-set2)))
      (assertion-violation 'enum-set-intersection "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

(define (enum-set-difference enum-set1 enum-set2)
  (define (difference lst1 lst2)
    (let loop ([ret '()]
               [lst lst1])
      (if (null? lst)
          ret
          (cond
           [(memq (car lst) lst2)
            (loop ret (cdr lst))]
           [else
            (loop (cons (car lst) ret) (cdr lst))]))))
  (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
      (make-enum-set (enum-set-type enum-set1)
                     (difference (enum-set-members enum-set1) (enum-set-members enum-set2)))
      (assertion-violation 'enum-set-difference "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

(define (enum-set-complement enum-set)
  (let ([members (enum-set-members enum-set)])
    (make-enum-set (enum-set-type enum-set)
                   (filter (lambda (symbol) (not (memq symbol members))) (enum-type-universe (enum-set-type enum-set))))))

(define (enum-set-projection enum-set1 enum-set2)
  (if (enum-set-subset? enum-set1 enum-set2)
      enum-set1
      (let ([universe2 (enum-type-universe (enum-set-type enum-set2))]
            [members1 (enum-set-members enum-set1)])
        (make-enum-set (enum-set-type enum-set2)
                       (filter (lambda (symbol) (memq symbol universe2)) members1)))))
