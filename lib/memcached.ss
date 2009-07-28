; memcached.ss
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
;  $Id: concurrent.ss 621 2008-11-09 06:22:47Z higepon $

;; Memcached client API.
;; This library is undocumented, APIs is subject to change without notice.
(library (memcached)
         (export
          memcached-connect
          memcached-set!
          memcached-get
          memcached-gets
          memcached-bv-set!
          memcached-bv-gets)
         (import (rnrs)
                 (mosh)
                 (only (srfi :1) alist-cons)
                 (only (srfi :13) string-join)
                 (mosh socket))

(define-record-type memcached-client
  (fields
   (immutable socket)))


(define (memcached-set! conn key flags expiry value)
  (let-values (([port get] (open-bytevector-output-port)))
    (fasl-write value port)
    (memcached-bv-set! conn key flags expiry (get))))

(define (memcached-get conn key)
  (let ([ret (assoc key (memcached-gets conn key))])
    (if ret
        (cdr ret)
        #f)))

(define (memcached-gets conn . key*)
  (map (lambda (key-value) (cons (car key-value)
                            (if (zero? (bytevector-length (cdr key-value)))
                                #f
                                (fasl-read (open-bytevector-input-port )))))
       (apply memcached-bv-gets conn key*)))

(define (memcached-bv-get conn key)
  (let ([ret (assoc key (memcached-bv-gets conn key))])
    (if ret
        (cdr ret)
        #f)))


(define (memcached-connect server port)
  (make-memcached-client (make-client-socket server port)))

(define (memcached-recv conn)
  (let ([buffer-size 4096]
        [socket (memcached-client-socket conn)])
      (let loop ([ret (make-bytevector 0)]
                 [data (socket-recv socket buffer-size)])
        (let* ([total-size (+ (bytevector-length ret) (bytevector-length data))]
               [new (make-bytevector total-size)])
          (bytevector-copy! ret 0 new 0 (bytevector-length ret))
          (bytevector-copy! data 0 new (bytevector-length ret) (bytevector-length data))
          (if (= (bytevector-length data) buffer-size)
              (loop new (socket-recv socket buffer-size))
              new)))))

(define (memcached-send conn text)
  (memcached-send-bv conn (string->utf8 text)))

(define (memcached-send-bv conn bv)
  (socket-send (memcached-client-socket conn) bv))

(define (memcached-bv-set! conn key flags expiry bv-value)
    (memcached-send conn (format "set ~a 0 0 ~d\r\n" key (bytevector-length bv-value)))
    (memcached-send-bv conn bv-value)
    (memcached-send conn "\r\n")
    (memcached-recv conn))

(define (memcached-bv-gets conn . keys)
  (memcached-send conn (format "get ~a\r\n" (string-join keys " ")))
  (parse-response (memcached-recv conn)))

(define (parse-response res)
  (let loop ([i 0]
             [ret '()])
    ;; END of response?
    (if (and (= (bytevector-u8-ref res i) (char->integer #\E))
             (let-values (([token-found? token-start token-end] (token-until-next-char res i #\return)))
               (and token-found?
                    (bytevector-eqv? res token-start (string->utf8 "END") 0 (string-length "END")))))
        (reverse ret)
        ;; VALUE
        (let-values (([token-found? token-start token-end] (token-until-next-space res i)))
          (unless token-found?
            (error 'parse-res "malformed response : VALUE expected"))
          (unless (bytevector-eqv? res token-start (string->utf8 "VALUE") 0 (string-length "VALUE"))
            (error 'parse-res "malformed res : VALUE expected"))
          ;; Key
          (let-values (([token-found? token-start token-end] (token-until-next-space res (+ token-end 2))))
            (unless token-found?
              (error 'parse-res "malformed response : Key expected"))
            (let ([key (partial-bytevector->string res token-start token-end)])
            ;; flags
            (let-values (([token-found? token-start token-end] (token-until-next-space res (+ token-end 2))))
              (unless token-found?
                (error 'parse-res "malformed response : flags expected"))
              ;; length of value terminate with \r\n
              (let-values (([token-found? token-start token-end] (token-until-next-char res (+ token-end 2) #\return)))
                (unless token-found?
                  (error 'parse-res "malformed response : length of value expected"))
                (let ([value-length (string->number (partial-bytevector->string res token-start token-end))])
                  (loop (+ (+ token-end 2 value-length) 3)
                        (cons (cons key (partial-bytevector res
                                                            (+ token-end 3) ;; skip \n
                                                            (+ token-end 2 value-length))) ret)))))))))))

(define (token-until-next-space bv start)
  (token-until-next-char bv start #\space))

(define (token-until-next-char bv start char)
  (let ([len (bytevector-length bv)])
    (let loop ([i start])
      (cond
       [(= len i)
        ;; End of bytevector
        (values #f start (- len 1))]
       [(= (bytevector-u8-ref bv i) (char->integer char))
        ;; char found
        ;; Returns (values found token-start token-end)
        (values #t start (- i 1))]
       [else
        (loop (+ i 1))]))))

(define (bytevector-eqv? bv1 bv1-start bv2 bv2-start len)
  (let ([len1 (bytevector-length bv1)]
        [len2 (bytevector-length bv2)])
    (let loop ([i bv1-start]
               [j bv2-start])
      (cond
       [(= len (- i bv1-start)) #t]
       [(and (= i len1) (= j len2)) #t]
       [(= i len1) #f]
       [(= j len2) #f]
       [(= (bytevector-u8-ref bv1 i) (bytevector-u8-ref bv2 j))
        (loop (+ i 1) (+ j 1))]
       [else #f]))))

(define (partial-bytevector bv start end)
  (let loop ([i start]
             [ret '()])
    (cond
     [(= i (+ end 1))
      (u8-list->bytevector (reverse ret))]
     [else
      (loop (+ i 1) (cons (bytevector-u8-ref bv i) ret))])))

(define (partial-bytevector->string bv start end)
  (utf8->string (partial-bytevector bv start end)))

)
