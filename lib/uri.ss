; uri.ss
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
(library (uri)
  (export uri-encode)
  (import (rnrs))

;; This library is undocumented. APIs is subject to change without notice.

;; http://tips.lisp-users.org/scheme/
(define uri-unreserved-char-sv?
  (let ((unreserved-char-svs (map char->integer
                                  (string->list
                                   "abcdefghijklmnopqrstuvwxyz¥
                                    ABCDEFGHIJKLMNOPQRSTUVWXYZ¥
                                    0123456789-._‾"))))
    (lambda (sv)
      (and (memv sv unreserved-char-svs) #t))))

(define (uri-encode str)
  (let ((svs (bytevector->u8-list (string->utf8 str))))
    (call-with-string-output-port
     (lambda (p)
       (for-each
        (lambda (sv)
          (cond ((uri-unreserved-char-sv? sv)
                 (display (integer->char sv) p))
                (else
                 (display "%" p)
                 (display (number->string sv 16) p))))
        svs)))))
)
