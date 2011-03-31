; mecab.ss : libmecab bindings.
;
;   Copyright (c) 2011  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
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
(library (mecab)
  (export mecab-new2
          mecab-sparse-tostr2
          mecab-sparse-tonode2
          mecab-node-surface
          mecab-node-feature
          mecab-node-surface*
          mecab-node-length
          mecab-node-next
          mecab-node-prev
          mecab-destroy)
  (import (rnrs)
          (mosh ffi)
          (mosh control)
          (only (srfi :13) string-null?)
          (mosh))

;; This library is undocumented. APIs is subject to change without notice.
;; Thanks to naoya_t.

;; We assume utf8

(define libmecab (open-shared-library "/usr/lib/libmecab.so"))

(define mecab-new2
  (c-function libmecab void* mecab_new2 char*))

(define mecab-destroy
  (c-function libmecab void mecab_destroy void*))

(define (mecab-sparse-tostr2 . args)
  (pointer->string (apply (c-function libmecab void* mecab_sparse_tostr void* char* int) args)))

(define mecab-sparse-tonode2
  (c-function libmecab void* mecab_sparse_tonode2 void* char* int))

(define (mecab-node-surface node)
  (pointer->string (pointer-ref-c-pointer node 8)
                   (mecab-node-length node)))

(define (mecab-node-surface* node)
  (let loop ([node node]
             [ret '()])
      (cond
       [(pointer-null? node)
        (reverse ret)]
       [else
        (let1 surface (mecab-node-surface node)
          (loop (mecab-node-next node)
                (if (string-null? surface) ret (cons (mecab-node-surface node) ret))))])))

(define (mecab-node-prev node) (pointer-ref-c-pointer node 0))
(define (mecab-node-next node) (pointer-ref-c-pointer node 1))
(define (mecab-node-enext node) (pointer-ref-c-pointer node 2))
(define (mecab-node-bnext node) (pointer-ref-c-pointer node 3))

(define (mecab-node-length node)
  (pointer-ref-c-unsigned-short node (+ (* (/ size-of-pointer size-of-short) 10) (/ size-of-unsigned-int size-of-short))))

(define (mecab-node-feature node-ptr)
  (map (lambda (s) (if (string=? "*" s) #f s))
       (string-split (pointer->string (pointer-ref-c-pointer node-ptr 9)) #\,)))

)
