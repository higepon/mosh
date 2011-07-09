;   fb-feed-get: Get facebook feed.
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
        (srfi :2)
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

(define (clean-body b)
  (regexp-replace-all #/\n/ b ""))

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
        (format p "~a$~a$~a$~a$~a$~a$"
                (assoc-ref (vector->list (assoc-ref m "from")) "id")
                (assoc-ref (vector->list (assoc-ref m "from")) "name")
                (clean-body (assoc-ref m "message"))
                (if (assoc-ref m "likes") (assoc-ref (vector->list (assoc-ref m "likes")) "count") 0)
                (assoc-ref m "id")
                (if (assoc-ref m "comments") (assoc-ref (vector->list (assoc-ref m "comments")) "count") 0))
        (and-let*
            ([comment* (assoc-ref m "comments")]
             [comment* (assoc-ref (vector->list comment*) "data")])
          (for-each
           (^c
            (format p "~a:~a;" (assoc-ref (vector->list (assoc-ref (vector->list c) "from")) "id") (clean-body (assoc-ref (vector->list c) "message")))
            )
           (if comment* comment* '())))
        (newline p)
        ]
       [else '()]))
    json))))
