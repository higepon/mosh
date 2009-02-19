(import (rnrs)
        (mosh test))

(define (with-all-buffer-mode proc)
  (for-each proc (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))

;; open-file-input-port with transcoder
(with-all-buffer-mode
 (lambda (mode)
   (let ([port (open-file-input-port "./test/utf16.txt" (file-options) mode (make-transcoder (utf-16-codec)))])
     (test/t (input-port? port))
     (test* (read port) "あいう")
     (test/f (port-eof? port)) ;; #f for textual port
     (close-port port))))

;; open-bytevector-input-port
(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc")))
                                        (make-transcoder (utf-8-codec)))])
  (test* (read-char port) #\a)
  (test* (read-char port) #\b)
  (test* (read-char port) #\c)
  (test/t (eof-object? (read-char port))))

(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc"))))])
  (test* (get-u8 port) (char->integer #\a))
  (test* (get-u8 port) (char->integer #\b))
  (test* (get-u8 port) (char->integer #\c))
  (test/t (eof-object? (get-u8 port))))

;; custom input-port
(let* ([pos 0]
       [p (make-custom-binary-input-port
           "custom in"
           (lambda (bv start count)
             (if (= pos 16)
                 0
                 (begin
                   (set! pos (+ 1 pos))
                   (bytevector-u8-set! bv start pos)
                   1)))
           (lambda () pos)
           (lambda (p) (set! pos p))
           (lambda () 'ok))])
  (test/t (port-has-port-position? p))
  (test/t (port-has-set-port-position!? p))
  (test* (port-position p) 0)
  (test* (get-bytevector-n p 3) #vu8(1 2 3))
  (test* (port-position p) 3)
  (test* (lookahead-u8 p) 4)
  (test* (lookahead-u8 p) 4)
  (test* (port-position p) 3)
  (set-port-position! p 10)
  (get-bytevector-n p 2)
  (test* (get-bytevector-n p 2) #vu8(13 14))
  (test* (get-bytevector-n p 2) #vu8(15 16))
  (test* (get-bytevector-n p 2) (eof-object))
  (set-port-position! p 2)
  (test* (get-bytevector-n p 3) #vu8(3 4 5))
  (close-port p))

(test-end)
