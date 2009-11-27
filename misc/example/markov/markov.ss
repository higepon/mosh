(import (rnrs)
        (mosh)
        (only (srfi :1) append! first second third list-ref)
        (only (srfi :13) string-tokenize)
        (only (srfi :27) random-integer random-source-randomize! default-random-source)
        (mosh process)
        (mosh socket)
        (mosh control))

;; todo
;; 深さを考える。（無限ループにならないように）

(define (make-sentence dic first second)
  (define (rec first second depth)
    (cond
     [(= depth 25) '()]
     [(hashtable-ref dic (cons first second) #f)
      => (lambda (v)
           (let ([selected (list-ref v (random-integer (length v)))])
             (cons first (rec second selected (+ depth 1)))))]
     [else '()]))
  (let ([lst (rec first second 0)])
    (if (< (length lst) 3)
        #f
        (apply string-append lst))))


;; space seprated file => list of string
(define (file->sexp file)
  (call-with-input-file file
    (lambda (in)
      (display (string-split (get-line in) #\space)))))

(define (learn dic words)
  (cond
   [(null? (cddr words)) '()]
   [else
    (if (or (string=? (first words) "") (string=? (first words) "　"))
        (learn dic (cdr words))
        (let1 prefix (cons (first words) (second words))
          (aif (hashtable-ref dic prefix #f)
               (append! it (list (third words)))
               (hashtable-set! dic prefix (list (third words))))
          (learn dic (cdr words)))])))

(define (write-dic dic port)
  (display "(" port)
  (hashtable-for-each
   (lambda (key value)
     (write (cons key value) port))
   dic)
  (display ")" port))

(define (read-dic port)
  (let ([dic (make-hashtable equal-hash equal?)])
    (for-each
     (lambda (x)
       (hashtable-set! dic (car x) (cdr x)))
     (read port))
    dic))

(define (main args)
  (call-with-input-file (second args)
    (lambda (in)
      (let ([dic (make-hashtable equal-hash equal?)])
        (let loop ([line (get-line in)]
                   [i 1])
          (cond
           [(eof-object? line)
            (make-sentence dic "Mona" "は")
            (display "start writing\n")
            (call-with-port (open-output-file "./okuoku.dic")
              (lambda (port)
                (write-dic dic port)))
            (display "end writing2\n")
            ]
           [else
            (let ([words (string-split line #\space)])
              (when (> (length words) 2)
                (learn dic words))
              (format #t "learning ~d\r" i)
              (loop (get-line in) (+ i 1)))]))))))

(define (euc->string bv)
  (when (file-exists? "./nkf.tmp")
    (delete-file "./nkf.tmp"))
  (call-with-port (open-file-output-port "./nkf.tmp" (file-options no-fail))
    (lambda (out)
      (put-bytevector out bv)))
  (let-values ([(in out) (pipe)])
    (let-values ([(pid cin cout cerr) (spawn "nkf" '("-J" "-w" "./nkf.tmp") (list #f out #f))])
      (close-port out)
      (begin0
        (port->string (transcoded-port in (make-transcoder (utf-8-codec))))
        (close-port in)
        (waitpid pid)))))

(define (string->euc string)
  (when (file-exists? "./nkf.tmp2")
    (delete-file "./nkf.tmp2"))
  (call-with-port (transcoded-port (open-file-output-port "./nkf.tmp2" (file-options no-fail)) (native-transcoder))
    (lambda (out)
      (display string out)))
  (let-values ([(in out) (pipe)])
    (let-values ([(pid cin cout cerr) (spawn "nkf" '("-j" "-W" "./nkf.tmp2") (list #f out #f))])
      (close-port out)
      (begin0
        (let loop ([b (get-u8 in)]
                   [ret '()])
          (if (eof-object? b)
              (u8-list->bytevector (reverse ret))
              (loop (get-u8 in) (cons b ret))))
        (close-port in)
        (waitpid pid)))))

(define (collect-verb+noun text)
  (when (file-exists? "./mecab.tmp")
    (delete-file "./mecab.tmp"))
  (call-with-port (open-output-file "./mecab.tmp")
    (lambda (out) (display text out)))
  (call-with-port (open-string-input-port (spawn->string "mecab" '("./mecab.tmp")))
    (lambda (in)
      (let loop ([line (get-line in)] [verb '()] [noun '()])
        (cond
         [(eof-object? line) (values verb noun)]
         [(#/([^\s]+)\s+動詞/ line)
          => (lambda (m)
               (let* ([next-line (get-line in)]
                      [match (#/([^\s]+)\s/ next-line)])
                 (cond
                  [(or (eof-object? next-line) (not match))
                   (loop next-line verb noun)]
                  [else
                   (loop (get-line in)
                         (cons (cons (m 1) (match 1)) verb)
                         noun)])))]
         [(#/([^\s]+)\s+名詞/ line)
          => (lambda (m)
               (let* ([next-line (get-line in)]
                      [match (#/([^\s]+)\s/ next-line)])
                 (cond
                  [(or (eof-object? next-line) (not match))
                   (loop next-line verb noun)]
                  [else
                   (loop (get-line in)
                         verb
                         (cons (cons (m 1) (match 1)) noun))])))]
         [else
          (loop (get-line in) verb noun)])))))

(define (irc-bot dic server port nick channel)
  (let ([socket (make-client-socket server port)])
    (define (send text)
      (assert (<= (string-length text) 510))
      (socket-send socket (string->euc (string-append text "\r\n"))))
    (define (recv)
      (euc->string (socket-recv socket 512)))
    (define (say text)
      (send (format "NOTICE ~a :~a" channel text)))
    (send (format "NICK ~a" nick))
    (send (format "USER ~a 0 * :~a" nick nick))
    (send (format "JOIN ~a" channel))
    (let loop ([data (recv)]
               [accum ""])
      (format #t "IRC:~s\n"  data)
      (cond
       [(#/:([^!]+).*PRIVMSG[^:]+:(.*)/ data) =>
        (lambda (m)
          (cond
           [(or (#/^こんにちは/ (m 2))
                (#/^こんばんは/ (m 2))
                (#/^ちは/ (m 2))
                (#/^はじめまして/ (m 2))
                (#/^こんばんわ/ (m 2))
                (#/^お初です/ (m 2))
                (#/^おはつです/ (m 2)))
              (let* ([x '("こんにちは" "ちは" "こにゃにゃちは" "ノシ" "こんばんは" "こんばんわ")]
                     [hello (list-ref x (random-integer (length x)))])
              (say hello)
              (loop (recv) accum))]
           [(> (random-integer 10) 2)
            (format #t "LOG: take a rest \n" )
            (loop (recv) (string-append accum (m 2)))]
           [else
          (let-values (([verb noun] (collect-verb+noun accum)))
            (format #t "LOG: verb=~a noun=~a\n" verb noun)
            (when (> (length noun) 0)
              (let* ([selected (list-ref noun (random-integer (length noun)))]
                     [sentence (make-sentence dic (car selected) (cdr selected))])
                (format #t "LOG: selected=~a sentence=~a\n" selected sentence)
                (cond
                 [sentence
                  (say sentence)
                  (loop (recv) (m 2))]
                 [else
                  (loop (recv) (append accum (m 2)))]))))]))]
       [(#/^ERROR.*/ data)
        (error 'IRC data)]
       [(#/^PING/ data)
        (send "PONG 0")]
       [(#/:.*433.*Nickname is already in use.*/ data)
        (error 'irc "Nickname is already in use")])
      (loop (recv) accum))
    (socket-close socket)))



(define (main2 args)
(random-source-randomize! default-random-source) ;
  (call-with-input-file (second args)
    (lambda (in)
      (let ([dic (read-dic in)])
        (irc-bot dic "irc.nara.wide.ad.jp" "6668" "yuki-bot" "#osdev-j")))))

;;         (let loop ([line (get-line (current-input-port))])
;;           (let-values (([verb noun] (collect-verb+noun line)))
;;             (when (> (length noun) 0)
;;               (let ([selected (list-ref noun (random-integer (length noun)))])
;;                 (format #t "line=~a selected=~a verb=~a noun\n" line selected verb noun)
;;                 (make-sentence dic (car selected) (cdr selected))))
;;           (call-with-port (open-output-file "./hige")
;;             (lambda (out) (display line out)))
;;           (call-with-port (open-string-input-port (spawn->string "mecab" '("hige")))
;;             (lambda (in)
;;               (make-sentence dic ((#/([^\s]+)\s/ (get-line in)) 1) ((#/([^\s]+)\s/ (get-line in)) 1))))
;          (loop (get-line (current-input-port)))))))))

(define (spawn->string command args)
  (let-values ([(in out) (pipe)])
    (let-values ([(pid cin cout cerr) (spawn command args (list #f out #f))])
      (close-port out)
      (begin0
        (port->string (transcoded-port in (make-transcoder (utf-8-codec))))
        (close-port in)
        (waitpid pid)))))

(define (port->string p)
  (let loop ([ret '()][c (read-char p)])
    (if (eof-object? c)
        (list->string (reverse ret))
        (loop (cons c ret) (read-char p)))))

(main2 (command-line))

;(main (command-line))


;(put-bytevector (standard-error-port) (string->euc "あいう") 0)
;(put-bytevector (standard-output-port) (string->euc "あいう"))


;; (let-values (([verb noun] (collect-verb+noun "こんにちは今日は散歩に行きましょう")))
;;   (format #t "verb = ~a noun=~a\n" verb noun))
