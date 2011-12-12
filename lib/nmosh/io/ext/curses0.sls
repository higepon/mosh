(library (nmosh io ext curses0)
         (export curses-acquire
                 screen-create
                 screen-set-title
                 screen-clear
                 screen-put)
         (import (rnrs)
                 (only (scheme base) list-set!)
                 (yuni core)
                 (srfi :26)
                 (srfi :42)
                 (shorten)
                 (match)
                 (nmosh io core)
                 (nmosh io master-queue)
                 (nmosh aio platform)
                 (nmosh pffi ext curses))

(define keyloop (mcur_getkeyloop))
(define key-resize (mcur_key_resize))
(define key-mouse (mcur_key_mouse))
(define pdcurses? (mcur_pdcurses))

(define attr-reverse (mcur_attrib_reverse))
(define (set-reverse) (mcur_attron attr-reverse))
(define (unset-reverse) (mcur_attroff attr-reverse))

(define* screen
  (buffer 
    input-buffer
    ;; cur-x and cur-y is for output buffer.
    cur-x
    cur-y
    width height))

(define (split-newline str)
  (define (itr acc cur rest)
    (match rest
           ((#\newline . rest)
            (itr '() (cons (list->string (reverse acc)) cur) rest))
           ((something . rest)
            (itr (cons something acc) cur rest))
           (else
             (reverse (cons (list->string (reverse acc)) cur)))))
  (itr '() '() (string->list str)))

(define (%screen-addstring screen str)
  (let-with screen (cur-x cur-y width height buffer)
    (define curline (list-ref buffer cur-y))
    (define curline-len (string-length curline))
    (define len (string-length str))
    (define rest-len (- width cur-x))
    (cond 
      ((<= len rest-len)
       ;; short cut
       (list-set! buffer cur-y (string-append curline str))
       (~ screen 'cur-x := (+ cur-x len)))
      (else
        (let ((p (substring str 0 rest-len))
              (s (substring str rest-len len)))
          (list-set! buffer cur-y (string-append curline p))
          (%screen-newline screen)
          (%screen-addstring screen s))))))

(define (%screen-scroll1 screen)
  ;; scroll down 1 line
  (touch! screen
          (buffer (append (cdr (~ screen 'buffer)) (list "")))))

(define (%screen-newline screen)
  (let-with screen (cur-y height)
    (~ screen 'cur-x := 0)
    (cond 
      ((= cur-y (- height 1))
       (%screen-scroll1 screen))
      (else
        (~ screen 'cur-y := (+ cur-y 1))))))

(define (%screen-addline screen str)
  (%screen-addstring screen str)
  (%screen-newline screen))

(define* (screen-put (screen) str)
  (define rl (reverse (split-newline str)))
  (define lastline (car rl))
  (define l (reverse (cdr rl)))
  (for-each (cut %screen-addline screen <>) l)
  (%screen-addstring screen lastline)
  (%screen-draw screen))

(define* (%screen-draw (screen))
  (let-with screen (cur-x cur-y width height buffer input-buffer)
    (mcur_cls)
    (mcur_locate 0 0)
    (let ((offset ;; Scroll up if editing line doesn't fit screen width
            (if (>= width (+ cur-x (string-length input-buffer))) 0 1)))

      ;; draw each line
      (do-ec (: l height)
             (begin
               (mcur_locate 0 l)
               (let ((x (+ l offset)))
                 (unless (= x height)
                   (mcur_print (list-ref buffer x))))))

      ;; draw edit area
      (set-reverse) 
      (cond
        ((= offset 0)
         ;; Just draw it
         (mcur_locate cur-x cur-y)
         (mcur_print input-buffer))
        ((= offset 1)
         ;; FIXME: Split into 2 lines..
         (mcur_locate cur-x cur-y)
         (mcur_print input-buffer))) 
      (unset-reverse)))
  (mcur_refresh))

(define* (screen-clear (screen))
  (~ screen 'buffer := (list-ec (: i (~ screen 'height)) ""))
  (mcur_cls))

(define (screen-set-title bogus title)
  (cond
    (pdcurses?
      (mcur_set_title title))
    (else ;; FIXME: Try ansi
      'ok
      )))

(define (screen-create callback) ;; => screen
  ;; callback = ^[type arg0 arg1 arg2 arg3]
  ;;  type = key | mouse? | ...
  ;;  key = arg0:char/sym

  (define (translate x)
    (if (< x 128)
      (case x
        ((3)
         'INT)
        (else
          (integer->char x)))
      x))
  (define (handler type arg0 arg1 arg2 arg3)
    (case type
      ((key)
       (cond
         ((= arg0 key-resize)
          (when pdcurses?
            (mcur_resize_term))
          ;; Resize screen buffer
          (mcur_refresh))
         (else
           (callback 'key (translate arg0) #f #f #f))))
      (else ;; unknown
        'ok)))
  (curses-acquire handler)
  (let ((r (make screen
                 (cur-x 0)
                 (cur-y 0)
                 (input-buffer "")
                 (height (mcur_lines))
                 (width (mcur_cols))))) 
    (screen-clear r)
    r))

(define (curses-acquire callback)
  (define (cb ret chr)
    (callback 'key chr #f #f #f))
  (mcur_acquire)
  (queue-invoke-ffithread nmosh-io-master-queue
                          keyloop
                          0
                          0
                          cb))

)
