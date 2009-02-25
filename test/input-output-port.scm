; input-output-port.scm - Tests for <input/output port>
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
;  $Id: test.ss 621 2008-11-09 06:22:47Z higepon $

(import (rnrs)
        (mosh)
        (mosh test)
        (mosh shell))
(def-command cp)

(define (with-all-buffer-mode proc)
  (cp "./test/test.txt" "./test/test.txt.temp")
  (for-each proc (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))



(with-all-buffer-mode
 (lambda (mode)
   (let ([port  (open-file-input/output-port "./test/test.txt.temp" (file-options) mode)])
     (test/t (input-port? port))
     (test/t (port-has-set-port-position!? port))
     (test/t (port-has-port-position? port))
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 1)
     (put-u8 port #x2e)
     (test* (get-u8 port) #x20)
     (test* (port-position port) 3)
     (set-port-position! port 8193) ;; over the buffer boundary
     (test* (port-position port) 8193)
     (test* (get-u8 port) 46)
     (set-port-position! port 8190)
     (test* (port-position port) 8190)
     (test* (get-u8 port) 32)
     (test* (lookahead-u8 port) 110)
     (test* (port-position port) 8191)
     (test* (get-u8 port) 110)
     (set-port-position! port 8190)
     ;; read over the boundary
     (test* (get-bytevector-n port 30) #vu8(32 110 50 46 116 111 70 108 111 110 117 109 40 41 41 59 10 32 32 32 32 125 32 101 108 115 101 32 123 10))
     (test* (port-position port) 8220)
     ;; read over the boundary and size > buffer-size
     (set-port-position! port 4000)
     (let ([bv1 (make-bytevector 10000)]
           [bv2 (get-bytevector-n port 10000)])
       (test* (bytevector-u8-ref bv2 0) 123)
       (test* (bytevector-u8-ref bv2 9999) 108)
       (set-port-position! port 4000)
       (test* (get-bytevector-n! port bv1 0 10000) 10000)
       (test/t (equal? bv1 bv2)))
     ;; read-some
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-some port)])
       (test/t (> (bytevector-length bv) 0))
       (test* (bytevector-u8-ref bv 0) 123))
     ;; read-all
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-all port)])
       (test* (bytevector-length bv) 34861)
       (test* (bytevector-u8-ref bv 0) 123)
       (test* (bytevector-u8-ref bv 34860) 10))
     (test* (port-position port) 38861)
     (close-port port))

   ;; check the written data
   (let ([port  (open-file-input/output-port "./test/test.txt.temp" (file-options) mode)])
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 1)
     (test* (get-u8 port) #x2e)
     (test* (port-position port) 2))
   ))


(test-end)
