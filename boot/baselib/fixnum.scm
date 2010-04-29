; fixnum.scm - fixnum procedures.
;
;   Copyright (c) 2008  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
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

(define (fxdiv-and-mod fx1 fx2)
  (values (fxdiv fx1 fx2) (fxmod fx1 fx2)))

(define (fxdiv0-and-mod0 fx1 fx2)
  (values (fxdiv0 fx1 fx2) (fxmod0 fx1 fx2)))

(define (fx+/carry fx1 fx2 fx3)
  (or (fixnum? fx1) (assertion-violation 'fx+/carry (format "fixnum required, but got ~a" fx1) fx1 fx2 fx3))
  (or (fixnum? fx2) (assertion-violation 'fx+/carry (format "fixnum required, but got ~a" fx2) fx1 fx2 fx3))
  (or (fixnum? fx3) (assertion-violation 'fx+/carry (format "fixnum required, but got ~a" fx3) fx1 fx2 fx3))
  (let* ((s (+ fx1 fx2 fx3))
         ;(s0 (mod0 s (expt 2 (fixnum-width))))
         (s0 (mod0 s (abs (+ (least-fixnum) (least-fixnum)))))
         ;(s1 (div0 s (expt 2 (fixnum-width)))))
         (s1 (div0 s (abs (+ (least-fixnum) (least-fixnum))))))
    (values s0 s1)))

(define (fx-/carry fx1 fx2 fx3)
  (or (fixnum? fx1) (assertion-violation 'fx-/carry (format "fixnum required, but got ~a" fx1) fx1 fx2 fx3))
  (or (fixnum? fx2) (assertion-violation 'fx-/carry (format "fixnum required, but got ~a" fx2) fx1 fx2 fx3))
  (or (fixnum? fx3) (assertion-violation 'fx-/carry (format "fixnum required, but got ~a" fx3) fx1 fx2 fx3))
  (let* ((d (- fx1 fx2 fx3))
         ;(d0 (mod0 d (expt 2 (fixnum-width))))
         (d0 (mod0 d (abs (+ (least-fixnum) (least-fixnum)))))
         ;(d1 (div0 d (expt 2 (fixnum-width)))))
         (d1 (div0 d (abs (+ (least-fixnum) (least-fixnum))))))
    (values d0 d1)))

(define (fx*/carry fx1 fx2 fx3)
  (or (fixnum? fx1) (assertion-violation 'fx*/carry (format "fixnum required, but got ~a" fx1) fx1 fx2 fx3))
  (or (fixnum? fx2) (assertion-violation 'fx*/carry (format "fixnum required, but got ~a" fx2) fx1 fx2 fx3))
  (or (fixnum? fx3) (assertion-violation 'fx*/carry (format "fixnum required, but got ~a" fx3) fx1 fx2 fx3))
  (let* ((s (+ (* fx1 fx2) fx3))
         ;(s0 (mod0 s (expt 2 (fixnum-width))))
         (s0 (mod0 s (abs (+ (least-fixnum) (least-fixnum)))))
         ;(s1 (div0 s (expt 2 (fixnum-width)))))
         (s1 (div0 s (abs (+ (least-fixnum) (least-fixnum))))))
    (values s0 s1)))

