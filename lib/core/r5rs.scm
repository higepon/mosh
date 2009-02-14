; r5rs.scm - r5rs procedures.
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
;  $Id: r5rs.scm 621 2008-11-09 06:22:47Z higepon $

;; Standard Libraries:19 R5RS compatibility

(define inexact->exact exact)
(define exact->inexact inexact)

;; Follwing procedures are rewrote in C++ for performance reason.
;; (define (sign n)
;;   (cond
;;    ((negative? n) -1)
;;    ((positive? n) 1)
;;    (else 0)))

;; (define (quotient n1 n2)
;;   (* (sign n1) (sign n2) (div (abs n1) (abs n2))))

;; (define (remainder n1 n2)
;;   (* (sign n1) (mod (abs n1) (abs n2))))

;; (define (modulo n1 n2)
;;   (* (sign n2) (mod (* (sign n2) n1) (abs n2))))

(define (force object)
  (object))

(define (make-promise proc)
  (let ((result-ready? #f)
        (result #f))
    (lambda ()
      (if result-ready?
          result
          (let ((x (proc)))
            (if result-ready?
                result
                (begin (set! result-ready? #t)
                       (set! result x)
                       result)))))))
