; bitwise.scm - bitwise procedures.
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
;  $Id: bitwise.scm 621 2008-11-09 06:22:47Z higepon $

(define (bitwise-if ei1 ei2 ei3)
  (bitwise-ior (bitwise-and ei1 ei2)
               (bitwise-and (bitwise-not ei1) ei3)))

(define (bitwise-bit-set? ei1 ei2)
  (not (zero? (bitwise-and (bitwise-arithmetic-shift-left 1 ei2) ei1))))

(define  (bitwise-copy-bit ei1 ei2 ei3)
  (let* ((mask (bitwise-arithmetic-shift-left 1 ei2)))
    (bitwise-if mask (bitwise-arithmetic-shift-left ei3 ei2) ei1)))

(define (bitwise-bit-field ei1 ei2 ei3)
  (when (> ei2 ei3)
    (assertion-violation 'bitwise-bit-field "2nd parameter must be less than or equal to 3rd parameter" ei1 ei2 ei3))
  (let ((mask (bitwise-not (bitwise-arithmetic-shift-left -1 ei3))))
    (bitwise-arithmetic-shift-right (bitwise-and ei1 mask) ei2)))

(define (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
  (let* ((to    ei1)
         (start ei2)
         (end   ei3)
         (from  ei4)
         (mask1 (bitwise-arithmetic-shift-left -1 start))
         (mask2 (bitwise-not (bitwise-arithmetic-shift-left -1 end)))
         (mask (bitwise-and mask1 mask2)))
    (bitwise-if mask (bitwise-arithmetic-shift-left from start) to)))

(define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
  (let* ((n     ei1)
         (start ei2)
         (end   ei3)
         (count ei4)
         (width (- end start)))
    (if (positive? width)
        (let* ((count (mod count width))
               (field0
                (bitwise-bit-field n start end))
               (field1 (bitwise-arithmetic-shift-left
                        field0 count))
               (field2 (bitwise-arithmetic-shift-right
                        field0
                        (- width count)))
               (field (bitwise-ior field1 field2)))
          (bitwise-copy-bit-field n start end field))
        n)))

;; Originally from Ypsilon Scheme
(define (bitwise-reverse-bit-field ei1 ei2 ei3)
  (let* ((n ei1)
         (start ei2)
         (end ei3)
         (width (- end start)))
    (if (positive? width)
        (let loop ((reversed 0) (field (bitwise-bit-field n start end)) (width width))
          (if (zero? width)
              (bitwise-copy-bit-field n start end reversed)
              (if (zero? (bitwise-and field 1))
                  (loop (bitwise-arithmetic-shift reversed 1)
                        (bitwise-arithmetic-shift-right field 1)
                        (- width 1))
                  (loop (bitwise-ior (bitwise-arithmetic-shift reversed 1) 1)
                        (bitwise-arithmetic-shift-right field 1)
                        (- width 1)))))
        n)))



