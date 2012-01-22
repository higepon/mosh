(library (yuni binary codec gdb)
         (export make-gdb-talker/target
                 gdb-reply-ok
                 gdb-reply-signal
                 gdb-reply-exit-status
                 gdb-reply-trap
                 gdb-reply-error
                 gdb-reply-registers
                 gdb-reply-memory
                 gdb-reply-threadinfo/id
                 gdb-reply-threadinfo/terminate
                 gdb-reply-current-thread)
         (import (rnrs) 
                 (rnrs r5rs)
                 (srfi :8)
                 (srfi :42))

;;; GDB Packets

(define-syntax ascii
  (syntax-rules ()
    ((_ x) (char->integer x)))) 

(define (nibble i)
  (case i
    ((0) (ascii #\0))
    ((1) (ascii #\1))
    ((2) (ascii #\2))
    ((3) (ascii #\3))
    ((4) (ascii #\4))
    ((5) (ascii #\5))
    ((6) (ascii #\6))
    ((7) (ascii #\7))
    ((8) (ascii #\8))
    ((9) (ascii #\9))
    ((10) (ascii #\A))
    ((11) (ascii #\B))
    ((12) (ascii #\C))
    ((13) (ascii #\D))
    ((14) (ascii #\E))
    ((15) (ascii #\F))))

;; FIXME
(define (nibble-num c)
  (cond
    ((= c (ascii #\0)) 0)
    ((= c (ascii #\1)) 1)
    ((= c (ascii #\2)) 2)
    ((= c (ascii #\3)) 3)
    ((= c (ascii #\4)) 4)
    ((= c (ascii #\5)) 5)
    ((= c (ascii #\6)) 6)
    ((= c (ascii #\7)) 7)
    ((= c (ascii #\8)) 8)
    ((= c (ascii #\9)) 9)
    ((or (= c (ascii #\A)) (= c (ascii #\a))) 10)
    ((or (= c (ascii #\B)) (= c (ascii #\b))) 11)
    ((or (= c (ascii #\C)) (= c (ascii #\c))) 12)
    ((or (= c (ascii #\D)) (= c (ascii #\d))) 13)
    ((or (= c (ascii #\E)) (= c (ascii #\e))) 14)
    ((or (= c (ascii #\F)) (= c (ascii #\f))) 15)))

(define (set-nibble-hex! bv off i)
  (bytevector-u8-set! bv off (nibble i)))

(define (set-u8-hex! bv off i)
  (let ((up (quotient i 16))
        (down (remainder i 16)))
    (set-nibble-hex! bv off up)
    (set-nibble-hex! bv (+ 1 off) down)))

;;; Packet Chunker

(define START-PACKET (ascii #\$))
(define END-PACKET (ascii #\#))
(define ESCAPE (ascii #\}))
(define STAR (ascii #\*))
(define COLON (ascii #\:))
(define SEMICOLON (ascii #\;))
(define COMMA (ascii #\,))

(define BREAK 3)

(define ACK (ascii #\+))
(define NACK (ascii #\-))
(define MINUS (ascii #\-))

(define ACK-packet (let ((bv (make-bytevector 1)))
                     (bytevector-u8-set! bv 0 ACK)
                     bv))

(define NACK-packet (let ((bv (make-bytevector 1)))
                      (bytevector-u8-set! bv 0 NACK)
                      bv))



(define (make-gdb-packet-chunker callback) ;; (^[byte])
  ;; callback <= #t (break) / #f (checksum error) / bytevector
  (define pktbuf-len)
  (define pktbuf)
  (define p)
  (define sum)
  (define sumr)
  (define state)
  (define (setpktbuf size)
    (let ((bv (make-bytevector size)))
      (bytevector-copy! pktbuf 0 bv 0 pktbuf-len)
      (set! pktbuf bv)
      (set! pktbuf-len size)))

  (define (packet len)
    (let ((bv (make-bytevector len)))
      (bytevector-copy! pktbuf 0 bv 0 len)
      bv))

  (set! pktbuf-len 1000)
  (set! pktbuf (make-bytevector pktbuf-len))
  (set! p 0)
  (set! sum 0)
  (set! state #f)
  (lambda (byte)
    ;; Receive $DDDD...#CC sequence
    (case state
      ((#f)
       ;; FIXME: handle ^C
       ;; Ignore non packet strings
       (cond
         ((= byte BREAK)
          (callback #t))
         ((= byte START-PACKET) 
          (set! state #t))))
      ((#t escape)
       (cond
         ((= byte END-PACKET)
          (set! state 'checksum0))
         (else
           (cond
             ((= byte ESCAPE)
              (set! state 'escape))
             (else
               (when (= pktbuf-len p)
                 (setpktbuf (+ p 1000))) 
               (bytevector-u8-set! pktbuf p (if (eq? state 'escape) 
                                              (bitwise-xor p #x20)
                                              byte)) 
               (set! p (+ p 1)) 
               (set! state #t)))
           ;; We have to include ESCAPE char to sum
           ;; and don't have to unescape 
           (set! sum (+ sum byte)) 
           (when (> sum 255)
             (set! sum (- sum 256))))))
      ((checksum0)
       (set! state 'checksum1)
       (set! sumr (nibble-num byte)))
      ((checksum1)
       (set! state #f)
       (let ((checksum (+ (* sumr 16) (nibble-num byte))))
         (let ((len p)
               (thispacketsum sum))
           (set! p 0)
           (set! sum 0)
           #|
           (display (list 'packet: (utf8->string (packet len)) 
                          'sum: checksum thispacketsum ))(newline)
           |#
           (callback (and (= checksum thispacketsum)
                          (packet len)))))))))

;;; Decompress

;;; Compress

;;; Escape
(define (escape-byte? c)
  (or (= START-PACKET c) (= END-PACKET c) 
      (= ESCAPE c) (= STAR c)))

(define (escape/checksum bv) ;; => bv
  (define sum 0)
  (define in-len (bytevector-length bv))
  (define len 4) ;; START + END + CHECKSUM(2bytes)
  (do-ec (: i in-len)
         (let ((c (bytevector-u8-ref bv i)))
           (cond
             ((escape-byte? c)
              (set! len (+ len 1))))))
  (set! len (+ in-len len))
  (let ((out (make-bytevector len)))
    (define (itr off idx)
      (define (update c)
        (bytevector-u8-set! out off c)
        (set! sum (+ sum c))
        (when (> sum 255)
          (set! sum (- sum 256)) ))
      (if (= idx in-len)
        off
        (let ((c (bytevector-u8-ref bv idx)))
          (cond
            ((escape-byte? c)
             (update out off ESCAPE)
             (update out (+ off 1) (bitwise-xor c #x20))
             (itr (+ off 2) (+ idx 1)))
            (else
              (update c)
              (itr (+ off 1) (+ idx 1)))))))
    (bytevector-u8-set! out 0 START-PACKET)
    (let ((next (itr 1 0)))
      (bytevector-u8-set! out next END-PACKET)
      (set-u8-hex! out (+ next 1) sum)) 
    (set! sum 0)
    out))

(define (fill-hex out off in)
  (do-ec (: i (bytevector-length in))
         (set-u8-hex! out (+ off (* 2 i))
                      (bytevector-u8-ref in i))))

(define (gdb-reply-registers bv)
  (let ((out (make-bytevector (* 2 (bytevector-length bv)))))
    (fill-hex out 0 bv) 
    ;(display (list 'from: bv))(newline)
    ;(display (list 'to: (utf8->string out)))(newline)
    (escape/checksum out)))

(define gdb-reply-memory gdb-reply-registers)

(define (gdb-reply-char ch)
  (let ((in (make-bytevector 1)))
    (bytevector-u8-set! in 0 (ascii ch))
    (escape/checksum in)))

(define (gdb-reply-letter ch val)
  (let ((in (make-bytevector 3)))
    (bytevector-u8-set! in 0 (ascii ch))
    (set-u8-hex! in 1 val)
    (escape/checksum in)))

(define (gdb-reply-ok)
  (escape/checksum (string->utf8 "OK")))

(define (gdb-reply-signal sig)
  (gdb-reply-letter #\S sig))

(define (gdb-reply-exit-status e)
  (gdb-reply-letter #\W e))

(define (gdb-reply-trap e)
  (gdb-reply-letter #\T e))

(define (gdb-reply-error e)
  (gdb-reply-letter #\E e))

(define (gdb-reply-threadinfo/terminate)
  (gdb-reply-char #\l))

(define (bv-concat . bv)
  (define total-len (fold-left + 0 (map bytevector-length bv)))
  (define out (make-bytevector total-len))
  (fold-left (lambda (pos bv)
               (let ((len (bytevector-length bv)))
                 (bytevector-copy! bv 0 out pos len) 
                 (+ pos len)))
             0
             bv)
  out)

(define (format-value val)
  (define (gen-base16 v)
    (define (itr acc cur)
      (if (= acc 0) 
        cur
        (itr (quotient acc 16)
             (cons (remainder acc 16) cur))))
    (itr v '()))
  (let ((o (gen-base16 val)))
    (u8-list->bytevector
    (if (null? o)
      (list (ascii #\0))
      (map nibble o)))))

(define (gdb-reply-threadinfo/id id)
  (escape/checksum
    (bv-concat
      (u8-list->bytevector (list (ascii #\m)))
      (format-value id))))

(define (gdb-reply-current-thread id)
  (escape/checksum
    (bv-concat
      (u8-list->bytevector (list (ascii #\Q)
                                 (ascii #\C)))
      (format-value id))))

;; Scan gdb queries.
;; fomart* = COMMA | SEMICOLON | COLON | MEMORY | VALUE
(define (gdb-scan bv off format)
  (define neg? #f)
  (define len (bytevector-length bv))
  (define (itr acc fmt sep off) ;; => value off
    ;; Read to sep, 
    (define byte (if (= len off) 'INVALID-VALUE (bytevector-u8-ref bv off)))
    (define (seperator?)
      (case sep
        ((#f) (= len off))
        ((COMMA) (= COMMA byte))
        ((SEMICOLON) (= SEMICOLON byte))
        ((COLON) (= COLON byte))
        (else (error 'itr "Invalid sep" sep))))
    (define (value)
      (case fmt
        ((MEMORY) (u8-list->bytevector (reverse acc)))
        ((VALUE) (if neg?  (- acc) acc))
        (else (assert #f))))
    (define (update)
      (case fmt
        ((MEMORY)
         (itr (cons (+ (* 16 (nibble-num byte))
                       (nibble-num (bytevector-u8-ref bv (+ off 1)))) acc) 
              fmt sep (+ off 2)))
        ((VALUE)
         (if (= byte MINUS)
           (begin 
             (set! neg? #t)
             (itr acc fmt sep (+ off 1)))
           (itr (+ (nibble-num byte) (* acc 16)) fmt sep (+ off 1))))
        (else (error 'itr "Invalid fmt" fmt))))
    (if (seperator?)
      (values (value) (+ off 1))
      (update)))

  (define (next off fmt cur)
    (if (pair? fmt)
      (let ((a (car fmt))
            (d (cdr fmt)))
        (let ((sep (if (pair? d) (car d) #f))
              (n (if (pair? d) (cdr d) d)))
          (receive (val off) (itr (case a
                                    ((VALUE) 0)
                                    ((MEMORY) '())) a sep off)
            (next off n (cons val cur)))))
      cur))
  (reverse (next off format '())))

(define-syntax dispatch-topchar
  (syntax-rules ()
    ((_ obj (char body ...) ...)
     (let ((c (bytevector-u8-ref obj 0)))
       (cond
         ((= (ascii char) c)
          body ...
          ) ...
         (else #f))))))

(define-syntax parse-header-string/clause
  (syntax-rules ()
    ((_ obj off ((string . param) head)) 
     (let* ((header-length (string-length string))
            (header-bytes (map (lambda (x) (ascii x)) (string->list string))))
       (and (<= header-length
                (- (bytevector-length obj) off))
            (for-all = (list-ec (: i header-length)
                                (bytevector-u8-ref obj (+ off i)))
                     header-bytes)
            (cons head '()) ;; FIXME: implement it.
            )))))

(define-syntax parse-header-string
  (syntax-rules ()
    ((_ obj off clause ...)
     (or (parse-header-string/clause obj off clause) ...))))

(define (parse-command/host bv)
  ;; Host commands
  ;;
  ;; g            (read-registers)
  ;; G            (write-registers bv)
  ;; m            (read-memory address size)
  ;; M            (write-memory address data)
  ;; c            (continue)
  ;; C            (continue/signal sig)
  ;; D            (detach)
  ;; s            (step)
  ;; S            (step/signal sig)
  ;;              (detatch)
  ;; k            (kill)
  ;; ?            (signal?)
  ;; qC           (current-thread?)
  ;; qfThreadInfo (threadinfo/first)
  ;; qsThreadInfo (threadinfo/next)
  ;; T            (thread-alive? id)
  ;; H?thead-id   (for-thread ID read-registers)
  ;;              (for-thread ID write-registers)

  (define (parse-query)
    (or
      (parse-header-string
        bv 1
        (("C") 'current-thread?)
        (("fThreadInfo") 'threadinfo/first)
        (("sThreadInfo") 'threadinfo/next))
      `(unknown-query ,(utf8->string bv))))

  (or
    (dispatch-topchar
      bv
      (#\? '(signal?))
      (#\g '(read-registers))
      (#\G `(write-registers . ,(gdb-scan bv 1 '(MEMORY))))
      (#\m `(read-memory . ,(gdb-scan bv 1 '(VALUE COMMA VALUE))))
      (#\M `(write-memory 
              .  ,(gdb-scan bv 1 '(VALUE COMMA VALUE COLON MEMORY))))
      (#\c '(continue))
      (#\C `(continue/signal ,(bytevector-u8-ref (car (gdb-scan bv 1 '(MEMORY))) 0)))
      (#\D '(detach))
      (#\s '(step))
      (#\S `(step/signal . ,(bytevector-u8-ref (car (gdb-scan bv 1 '(MEMORY))) 0)))
      (#\H `(for-thread ,(car (gdb-scan bv 2 '(VALUE))) 
                        ,(case (integer->char (bytevector-u8-ref bv 1))
                           ((#\g) 'read-registers)
                           ((#\G) 'write-registers)
                           (else 'UNKNOWN))))
      (#\k '(kill))
      (#\T `(thread-alive? . ,(gdb-scan bv 1 '(VALUE))))
      (#\q (parse-query)))
    '(unknown)))

(define (make-gdb-talker/target callback/send callback/event) 
  ;; callback/send = (^[bv] ...)
  ;; callback/event = (^[evt] ...)
  (define (make-packet-parser callback/event) ;; => (^[bv/bool] ...)
    (lambda (buffer)
      (case buffer
        ((#f) ;; Checksum error
         (callback/send NACK-packet))
        ((#t) ;; Break
         (callback/event '(break)))
        (else
          (callback/send ACK-packet)
          (callback/event (parse-command/host buffer))))))

  (define chunker (make-gdb-packet-chunker (make-packet-parser callback/event)))
  (lambda (buffer)
    (do-ec (: i (bytevector-length buffer))
           (chunker (bytevector-u8-ref buffer i)))))

)
