; io-error.scm - Tests for i/o errors
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
        (mosh process)
        (mosh shell)
        (mosh test))

(def-command chmod)

;; utf-8-codec
;;   error-handling-mode: raise
(test/exception i/o-decoding-error?
                (bytevector->string #vu8(97 #xff 98 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise))))

;; utf-8-codec
;;   error-handling-mode: ignore
(test* (bytevector->string #vu8(97 #xff 98 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode ignore)))
       "abc")

;; utf-8-codec
;;   error-handling-mode: ignore
(test* (bytevector->string #vu8(97 98 #xff 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode ignore)))
       "abc")

;; utf-8-codec
;;   error-handling-mode: ignore
(test* (bytevector->string #vu8(97 98 99 #xff) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode ignore)))
       "abc")

;; utf-8-code
;;   error-handling-mode: replace
(let ([s (bytevector->string #vu8(97 #xff 98 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode replace)))])
  (test* (string-ref s 0) #\a)
  (test* (string-ref s 1) (integer->char #xfffd))
  (test* (string-ref s 2) #\b)
  (test* (string-ref s 3) #\c))

;; How do I can test utf-8-codec encoding-error?
;; It never happen?

;; utf-8-codec
;;  read
(test/exception i/o-decoding-error?
                 (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                       (file-options no-truncate no-fail)
                                                       (buffer-mode none)
                                                       (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                         read))

(test/exception i/o-decoding-error?
                 (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                       (file-options no-truncate no-fail)
                                                       (buffer-mode none)
                                                       (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                         (lambda (x) (read x))))

;; utf-8-codec
;;  peek-char
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                peek-char))

;; utf-8-codec
;;  get-datum
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                get-datum))

;; utf-8-codec
;;  get-string
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                get-string-all))

;; utf-8-codec
;;  get-string-n!
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                (lambda (x) (get-string-n! x "abc" 0 3))))

;; utf-8-codec
;;  get-char
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                get-char))

;; utf-8-codec
;;  get-string-n
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                (lambda (x) (get-string-n x 3))))

;; utf-8-codec
;;  port-eof?
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                port-eof?))

;; utf-8-codec
;;  get-line
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                get-line))

;; utf-8-codec
;;  lookahead-char
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                lookahead-char))

;; utf-8-codec
;;  read-char
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                read-char))

;; utf-8-codec
;;  read-char
(test/exception i/o-decoding-error?
                (call-with-port (open-file-input-port "./test/invalid-utf8.txt"
                                                      (file-options no-truncate no-fail)
                                                      (buffer-mode none)
                                                      (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise)))
                (lambda (x) (read-char x))))


;; utf-16-codec
;;   error-handling-mode: raise
(test/exception i/o-decoding-error?
                (bytevector->string #vu8(97) (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise))))


;; utf-16-codec
;;   error-handling-mode: ignore
(test* (bytevector->string #vu8(97) (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode ignore)))
       "")

;; utf-16-code
;;   error-handling-mode: replace
(let ([s (bytevector->string #vu8(97) (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode replace)))])
  (test* (string-ref s 0) (integer->char #xfffd)))

;; utf-16-codec
;;  read
;; (test/exception i/o-decoding-error?
;;                  (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                        (file-options no-truncate no-fail)
;;                                                        (buffer-mode none)
;;                                                        (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                          read))

;; (test/exception i/o-decoding-error?
;;                  (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                        (file-options no-truncate no-fail)
;;                                                        (buffer-mode none)
;;                                                        (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                          (lambda (x) (read x))))

;; ;; utf-16-codec
;; ;;  peek-char
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 peek-char))

;; ;; utf-16-codec
;; ;;  get-datum
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 get-datum))

;; ;; utf-16-codec
;; ;;  get-string
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 get-string-all))

;; ;; utf-16-codec
;; ;;  get-string-n!
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 (lambda (x) (get-string-n! x "abc" 0 3))))

;; ;; utf-16-codec
;; ;;  get-char
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 get-char))

;; ;; utf-16-codec
;; ;;  get-string-n
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 (lambda (x) (get-string-n x 3))))

;; ;; utf-16-codec
;; ;;  port-eof?
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 port-eof?))

;; ;; utf-16-codec
;; ;;  get-line
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 get-line))

;; ;; utf-16-codec
;; ;;  lookahead-char
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 lookahead-char))

;; ;; utf-16-codec
;; ;;  read-char
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 read-char))

;; ;; utf-16-codec
;; ;;  read-char
;; (test/exception i/o-decoding-error?
;;                 (call-with-port (open-file-input-port "./test/invalid-utf16.txt"
;;                                                       (file-options no-truncate no-fail)
;;                                                       (buffer-mode none)
;;                                                       (make-transcoder (utf-16-codec) (native-eol-style) (error-handling-mode raise)))
;;                 (lambda (x) (read-char x))))


(test/exception i/o-invalid-position-error?
                (let ([port (open-file-input-port "./test/invalid-utf8.txt"
                                                  (file-options no-truncate no-fail)
                                                  (buffer-mode none))])
                  (set-port-position! port -1)))

;; file-is-read-only
(unless (sring=? (host-os) "win32")
  (chmod -w "./test/read-only.txt")
  (test/exception i/o-file-is-read-only-error?
                  (open-file-input/output-port "./test/read-only.txt" (file-options no-fail) 'block))

  (test/exception i/o-file-is-read-only-error?
                  (open-file-output-port "./test/read-only.txt" (file-options no-fail) 'block)))


; we can't "svn add" this file, but test is OK.
;(open-file-output-port "./test/can-not-read-write.txt" (file-options no-fail) 'block)
;(open-file-input/output-port "./test/can-not-read-write.txt" (file-options no-fail) 'block)
;(open-file-input-port "./test/can-not-read-write.txt" (file-options no-fail) 'block)
;(open-input-file "./test/can-not-read-write.txt")
;(open-output-file "./test/can-not-read-write.txt")


(test-end)


