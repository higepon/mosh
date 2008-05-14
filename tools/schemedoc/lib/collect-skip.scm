;;;; This library supports buffered input stream. A buffered input stream is convenient for various kinds of parsing tasks. <p>
;;;; It is assumed that the variable ip references an open input port, or a string. The assignment of ip must be done
;;;; exernally to this library, and after the library is loaded. If ip references an open input stream (made by the Scheme function
;;;; open-input-file, for instance) input is read from that port. If ip is string, the variable pstring-ip-pointer is used as a pointer
;;;; into the string.<p>
;;;; The functions in section one are generic, low-level reading functions that either read from the open input port, or from a string (at the location 
;;;; determined by the variable pstring-ip-pointer). The functions in section two are the basic reading and peeking functions. 
;;;; In section three a number of convenient collection and skipping functions
;;;; are provided for.<p>
;;;; This library has been developed as part of an SGML/XML Document Type Definition (DTD) parser, but
;;;; it is useful in many other parsing situations.
;;;; There exists <a href="http://www.cs.auc.dk/~normark/scheme/tools/dtd-parser/doc/html/index.html">som early internal documentation</a>
;;;; of the DTD parser (on the www.cs.auc.dk site)
;;;; and as such also of some aspects of the functions in this library.<p>
;;;; In earlier versions of LAML, this library was called 'the text collection and skipping library'.
;;;; .title Reference Manual of Buffered Input Streams


; An open input port, or a string.
(define ip #f)

; Only used if we use a string type ip. Points at the char to be read next.
(define pstring-ip-pointer 0);

; ========================================================================================================================
;;; Low-level, generic input functions.
;;; The functions in this section reads from either an input port, or from a string.

;; Reads a single character from ip, and advances the input pointer.
(define (generic-read-char ip)
  (cond ((input-port? ip) (read-char ip))
        ((string? ip) 
           (if (>= pstring-ip-pointer (string-length ip))
               #f
               (let ((res (string-ref ip pstring-ip-pointer)))
		 (set! pstring-ip-pointer (+ 1 pstring-ip-pointer))
		 res)))
        (else (laml-error "generic-read-char: ip must be a string or an input stream"))))

;; Is x the designated end-of-file value relative to the implicitly given input port ip.
(define (generic-eof-object? x)
  (cond ((input-port? ip) (eof-object? x))
        ((string? ip) (and (boolean? x) (not x)))
        (else (laml-error "generic-eof-object?: ip must be a string or an input stream"))))

; A non functional eof condition. Works only for string ip's
(define (generic-at-eof?)
 (cond  ((string? ip) (= pstring-ip-pointer (string-length ip)))
        (else (laml-error "generic-at-eof?: ip must be a string for this function to work"))))
    


; ========================================================================================================================

;;; Look ahead buffer and queue.
;;; The functions in this section manipulates a look ahead queue, which is in between the input port ip
;;; and the applications. Via this buffer it is possible to implement look ahead in the input port.<p>
;;; Imagine an input buffer of (actual) size n:
;;; <pre>    c1 c2 c3 ... cn </pre>
;;; When characters are read from the input stream, they enter to the right (the peek end).
;;; When characters are read by an application they are taken from the left (the read end).
;;; Thus, cn is the last character read from the input port (or from the input string); This is done by peek-a-char.
;;; c1 is the next char to leave the buffer, and to be read by the client application; This will be done by read-a-char.<p>
;;;
;;; A few words about terminology in relation to R4RS or R5RS. The Scheme procedure read-char corresponds roughtly to read-a-char;
;;; The former always reads a character from an input port; The latter reads from an input stream via the buffer; 
;;; Only if the buffer is empty, a character is read from the port or string.
;;; The proper Scheme function peek-char returns the next char from the input port, without updating the 'input pointer'.
;;; The function peek-a-char of this library is different, because it reads a character from the file and puts it into the buffer.
;;; This use of terminology is unfortunate, and it may be confusing for some readers.


;; The length of the cyclic look ahead buffer. Predefined to 20000 characters. A constant.
(define max-look-ahead 20000)

(define look-ahead-vector (make-vector max-look-ahead #\space))
(define next-write 0)
(define next-read 0)
(define look-ahead-length 0)

(define end-of-file? #f)

;; Reset the look ahead buffer. 
;; You should always call this function after you have re-assigned ip to a new input stream.
(define (reset-look-ahead-buffer)
  (set! ip #f)
  (set! pstring-ip-pointer 0)
  (set! next-write 0)
  (set! next-read 0)
  (set! look-ahead-length 0)
  (set! look-ahead-vector (make-vector max-look-ahead #\space))
  (set! end-of-file? #f)
  (set! collection-buffer (make-string buffer-length #\space))
)

(define (get-look-ahead-buffer)
  (pair-up
    '(    ip pstring-ip-pointer next-write next-read look-ahead-length look-ahead-vector end-of-file? collection-buffer buffer-length)
    (list ip pstring-ip-pointer next-write next-read look-ahead-length look-ahead-vector end-of-file? collection-buffer buffer-length)))

(define (put-look-ahead-buffer look-ahead-buffer-alist)
  (set! ip (get 'ip look-ahead-buffer-alist))
  (set! pstring-ip-pointer (get 'pstring-ip-pointer look-ahead-buffer-alist))
  (set! next-write (get 'next-write look-ahead-buffer-alist))
  (set! next-read (get 'next-read look-ahead-buffer-alist))
  (set! look-ahead-length (get 'look-ahead-length look-ahead-buffer-alist))
  (set! look-ahead-vector (get 'look-ahead-vector look-ahead-buffer-alist))
  (set! end-of-file? (get 'end-of-file? look-ahead-buffer-alist))
  (set! collection-buffer (get 'collection-buffer look-ahead-buffer-alist))
  (set! buffer-length (get 'buffer-length look-ahead-buffer-alist))
)


;; Peek a character from the input port, but queues it for subsequent reading
;; at "the peek end".
;; This function always reads one character via generic-read-char, and puts in into the "peek end" of the buffer.
(define (peek-a-char)
  (let ((ch (generic-read-char ip)))
     (if (generic-eof-object? ch)
         (begin 
            (set! end-of-file? #t)
            ch)
         (begin
           (vector-set! look-ahead-vector next-write ch)
           (set! next-write (+ 1 next-write))
           (set! look-ahead-length (+ 1 look-ahead-length))
           (if (> look-ahead-length max-look-ahead) (error "Lookahead buffer capacity exceeded"))
           (if (>= next-write max-look-ahead) (set! next-write 0))
           ch))))

;; Peeks n charcters, by n calls of peek-a-char. 
;; In other words, the buffer is extended with n characters read from the input stream.
;; .internal-references "relies on" "peek-a-char"
(define (peek-chars n)
  (cond ((> n 0)
          (begin
            (let ((ch (peek-a-char)))
              (if (not (generic-eof-object? ch)) (peek-chars (- n 1))))))
        ((< n 0) (error "peek-chars: Called with negative argument"))))

;; Read from the the look ahead buffer. Only if this buffer is empty, read from the port.
;; Reads from "the read end" of the queue.
;; In case the buffer is non-empty, this procedure takes a character out of the buffer in the "read end".
;; In any case, it advances the implicit input pointer of the input stream.
(define (read-a-char)
  (if (> look-ahead-length 0)
      (let ((ch (vector-ref look-ahead-vector next-read)))
        (set! next-read (+ next-read 1))
        (set! look-ahead-length (- look-ahead-length 1))
        (if (>= next-read max-look-ahead) (set! next-read 0))
        ch)
      (let ((ch (generic-read-char ip)))
        (if (generic-eof-object? ch)
            (set! end-of-file? #t))
        ch)))
            

;; Read and return a string of length n by means of repeated activations of read-a-char.
;; Takes eof into account such that a string shorter than n can be returned.
(define (read-a-string n)
  (let ((res (make-string n #\space)))
    (read-a-string-1 0 n res)
    res))

(define (read-a-string-1 i n str)
  (cond ((>= i n) str)
        (else (begin
                 (string-set! str i (read-a-char))
                 (read-a-string-1 (+ i 1) n str)))))

;; Return a string of length lgt from the "read end" of the buffer. A proper function.
;; .pre-condition lgt cannot be larger than the number of characters in the buffer.
(define (look-ahead-prefix lgt)
  (if (>= look-ahead-length lgt)
      (look-ahead-prefix-1 0 next-read lgt (make-string lgt #\space))
      (error (string-append "look-ahead-prefix: requires the look ahead to be in the queue, " (as-string lgt) ))))

; i is the index into the formed string.
; j is the index into the look-ahead queue
; lgt is the desired length of the extracted string
; res is the (tail recursive) result.
(define (look-ahead-prefix-1 i j n res)
  (if (>= i n)
      res
      (begin
         (string-set! res i (vector-ref look-ahead-vector j))
         (look-ahead-prefix-1
            (+ i 1)
            (if (= j (- max-look-ahead 1)) 0 (+ j 1)) ; @f
            n
            res))))

;; Return the entire look ahead queue as a string. A proper function.
(define (max-look-ahead-prefix)
  (look-ahead-prefix look-ahead-length))

;; Return the first character from the "read end" of the buffer. A proper function.
;; .pre-condition The buffer is not empty.
(define (look-ahead-char)
  (if (>= look-ahead-length 1)
      (vector-ref look-ahead-vector next-read)
      (error "look-ahead-char: Cannot look ahead in emtpy look ahead queue")))
  
;; Return whether the buffer matches the string str. Matching is done by the function equal? A proper function.
(define (match-look-ahead? str)
  (let* ((lgt (string-length str)))
    (if (>= look-ahead-length lgt)
        (equal? (look-ahead-prefix lgt) str)
        #f)))

;; Make sure that there is at least n characters in the buffer.
;; If there are less than n characters, ented a sufficient number of characters with peek-chars.
;; .internal-references "relies on" "peek-chars"
(define (ensure-look-ahead n)
  (if (< look-ahead-length n)
      (peek-chars (- n look-ahead-length))))

; ----------------------------------------------------------------------------
; Put back facility at the write end. Part of the look ahead queue.
; Alternatively - and more useful - put back should take place at the read end.

;; Put ch into the "peek end" of buffer (where peek-a-char operates).
(define (put-back-a-char-write-end ch)
  (vector-set! look-ahead-vector next-write ch)
  (set! next-write (+ 1 next-write))
  (set! look-ahead-length (+ 1 look-ahead-length))
  (if (> look-ahead-length max-look-ahead) (error "Lookahead buffer capacity exceeded"))
  (if (>= next-write max-look-ahead) (set! next-write 0)))

;; Put ch into the "read end" buffer (where read-a-char operates).
(define (put-back-a-char-read-end ch)
  (if (<= next-read 0) (set! next-read (- max-look-ahead 1)))
  (set! look-ahead-length (+ look-ahead-length 1))
  (if (>= look-ahead-length max-look-ahead) (error "Lookahead buffer capacity exceeded"))
  (set! next-read (- next-read 1))
  (vector-set! look-ahead-vector next-read ch))

;; Put str back into the buffer. The second parameter which-end controls whether to put back
;; in read end or write end (equivalent to peek end). Possible values of which end are the symbols read-end or write-end.
(define (put-back-a-string str which-end)
  (cond ((= 0 (string-length str)) 'nothing)
        ((eq? which-end 'write-end) 
           (put-back-a-string-write-end str 0 (- (string-length str) 1)))
        ((eq? which-end 'read-end)
           (put-back-a-string-read-end str 0 (- (string-length str) 1)))
        (else (error "put-back-a-string: Unknown end indicator"))))

(define (put-back-a-string-write-end str i max)
  (put-back-a-char-write-end (string-ref str i))
  (if (< i max)
      (put-back-a-string-write-end str (+ i 1) max)))

(define (put-back-a-string-read-end str min i)
  (put-back-a-char-read-end (string-ref str i))
  (if (> i min)
      (put-back-a-string-read-end str min (- i 1))))


;; Provided that there is at least n characters in the buffer, advance
;; next-read with n positions. Hereby n queued characters are skipped from the buffer at the "read end".
(define (advance-look-ahead n)
  (if (> n look-ahead-length) (error (string-append "Cannot advance the look ahead with " (as-string n) " positions")))
  (if (> n 0)
      (begin
        (set! next-read (+ next-read 1))
        (set! look-ahead-length (- look-ahead-length 1))
        (if (>= next-read max-look-ahead) (set! next-read 0))
        (advance-look-ahead (- n 1)))))

; End of look ahead buffer (queue)

; -----------------------------------------------------
;;; Collection and skipping functions.
;;; This section contains a number of higher level collection and skipping functions.
;;; These functions use the funtions from the previous section. The functions in this
;;; section are the most important of this library.

(define buffer-length 10000)
(define collection-buffer (make-string buffer-length #\space))

;; Read and collect a string from the input, controlled by a predicate.
;; The collection stops when the predicate p holds on the character read.
;; The last read character (the first character on which p holds) is left as the oldest character in the queue.
(define (collect-until p)
  (collect-until-1 p ip collection-buffer 0)
)

(define (collect-until-1 p ip buffer next)
  (cond ((>= next buffer-length) (error "collect-until-1: Collection buffer is filled. You can enlarge it via the variable buffer-length"))
        ((and (> look-ahead-length 0) (p (as-char (look-ahead-prefix 1))))
            (substring buffer 0 next))
        ((and (> look-ahead-length 0) (not (p (as-char (look-ahead-prefix 1)))))
          (let ((ch (read-a-char)))
             (string-set! buffer next ch)
             (collect-until-1 p ip buffer (+ 1 next))))
        ((and (= look-ahead-length 0))
          (let ((ch (peek-a-char)))
            (if (p ch)
                (substring buffer 0 next)
                (begin
                  (string-set! buffer next ch)
                  (read-a-char)
                  (collect-until-1 p ip buffer (+ 1 next))))))))

;; This collection procedure returns a balanced collection given two char predicates.
;; Return the string collected from the input port ip. The collection stops when the predicate char-pred-2 holds on the character read.
;; However, if char-pred-1 becomes true it has to be matched by char-pred-2 without causing a termination of the collection.
;; The last read character (the first character on which char-pred-2 holds) is processed by this function.
;; As a precondition assume that if char-pred-1 holds then char-pred-2 does not hold, and vice versa.
(define (collect-balanced-until char-pred-1 char-pred-2)
  (collect-balanced-until-1 char-pred-1 char-pred-2 ip collection-buffer 0 0))

(define (collect-balanced-until-1 q p ip buffer next bal-count)
  (ensure-look-ahead 1)
  (cond ((>= next buffer-length) (parse-error "collect-until-1: Collection buffer is filled. You can enlarge it via the variable buffer-length"))
        ((and (p (as-char (look-ahead-prefix 1))) (= bal-count 0))
            (parse-error "End delimitor matched before start delimitor"))
        ((and (p (as-char (look-ahead-prefix 1))) (= bal-count 1))
            (string-set! buffer next (read-a-char))
            (substring buffer 0 (+ next 1)))
        ((and (p (as-char (look-ahead-prefix 1))) (> bal-count 1))
          (let ((ch (read-a-char)))
             (string-set! buffer next ch)
             (collect-balanced-until-1 q p ip buffer (+ 1 next) (- bal-count 1))))
        ((and (q (as-char (look-ahead-prefix 1))))
          (let ((ch (read-a-char)))
             (string-set! buffer next ch)
             (collect-balanced-until-1 q p ip buffer (+ 1 next) (+ bal-count 1))))
        ((and (not (p (as-char (look-ahead-prefix 1)))) (not (q (as-char (look-ahead-prefix 1)))))
          (let ((ch (read-a-char)))
             (string-set! buffer next ch)
             (collect-balanced-until-1 q p ip buffer (+ 1 next) bal-count)))))



;; Skip characters while p holds.
;; The first character on which p fails is left as the oldest character in the queue.
;; The predicate does not hold if end of file.
(define (skip-while p)
  (cond ((and (not end-of-file?) (> look-ahead-length 0) (p (as-char (look-ahead-prefix 1)))) 
            (begin (read-a-char) (skip-while p)))
        ((and (not end-of-file?) (= look-ahead-length 0))
            (begin (peek-a-char)
                   (if (and (not end-of-file?) (p (as-char (look-ahead-prefix 1))))
                       (begin (read-a-char) (skip-while p)))))))
         
;; Assume that str is just in front of us. Skip through it.
;; If str is not in front of us, a fatal error occurs with if-not-message as error message.
(define (skip-string str if-not-message)
  (let ((str-1 (read-a-string (string-length str))))
    (if (not (equal? str str-1))
        (error if-not-message))))


;; Skip characters until str is encountered. If inclusive, also skip str.
;; It is assumed as a precondition that the length of str is at least one.
(define (skip-until-string str . inclusive)
  (let* ((str-lgt (string-length str))
         (first-ch (string-ref str 0))
         (incl (if (null? inclusive) #f (car inclusive))))
    (skip-until-string-1 str str-lgt first-ch incl)))

(define (skip-until-string-1 str str-lgt first-ch incl)
  (skip-while (negate (char-predicate first-ch)))
  (ensure-look-ahead str-lgt)
  (if (equal? (look-ahead-prefix str-lgt) str)
      (if incl (read-a-string str-lgt))
      (begin 
         (read-a-char) ; eat the matched first char
         (skip-until-string-1 str str-lgt first-ch incl))))


;; Collect characters until str is encountered. If inclusive, also collect str.
;; It is assumed as a precondition that the length of str is at least one.
(define (collect-until-string str . inclusive)
  (let* ((str-lgt (string-length str))
         (first-ch (string-ref str 0))
         (incl (if (null? inclusive) #f (car inclusive))))
    (collect-until-string-1 str str-lgt first-ch incl)))

(define (collect-until-string-1 str str-lgt first-ch incl)
  (let ((res (collect-until (char-predicate first-ch))))
    (ensure-look-ahead str-lgt)
    (if (equal? (look-ahead-prefix str-lgt) str)
        (if incl (string-append res (read-a-string str-lgt)) res)
        (string-append res (as-string (read-a-char)) (collect-until-string-1 str str-lgt first-ch incl)))))



; ----------------------------------------------
;;; Useful predicates for skipping and collecting.

;; Is ch a white space character?
(define (is-white-space? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
        (or (eqv? n 32) (eqv? n 9) (eqv? n 10) (eqv? n 12) (eqv? n 13)))))

;; Is ch an end of line charcter?
(define (end-of-line? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
         (or  (eqv? n 10) (eqv? n 13)))))

;; Is ch an end of file character?
(define (eof? ch)
  (generic-eof-object? ch))

;; Return a predicate functions which matches the character ch.
;; A higher order function.
(define (char-predicate ch)
  (lambda (c) (eqv? c ch)))

