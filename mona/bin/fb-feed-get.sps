;   fb-feed-get: Get facebook feed.
;
;   usage: http-get.sps uri path-to-file-to-save
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
(import (rnrs)
        (mosh control)
        (match)
        (mosh)
        (system)
        (shorten)
        (facebook))

(define fb-token
  (call-with-port
   (open-input-file (string-append (get-environment-variable "HOME") "/fb.token"))
   read))

(define temp-file (string-append (get-environment-variable "HOME") "/TEMP/fb.data"))


(when (file-exists? temp-file)
    (delete-file temp-file))

(let1 json (fb-news fb-token)
  (call-with-port
   (open-output-file temp-file)
   (^p
   (for-each
    (^(m)
      (cond
       [(and (assoc-ref m "message") (assoc-ref m "from"))
        (format p "~a\n~a\n" (assoc-ref (vector->list (assoc-ref m "from")) "name") (assoc-ref m "message"))]
       [else '()]))
    json))))
