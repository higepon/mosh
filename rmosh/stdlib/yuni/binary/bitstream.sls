(library (yuni binary bitstream)
         (export bitstream
                 port->bitstream
                 endianness ;; from (rnrs)
                 bitstream-read
                 bitstream-skip-alignment)
         (import (rnrs)
                 (yuni core))
(define* bitstream
  (live? 
    port width width/octet 
    read-count read-buffer write-buffer read-pointer write-pointer 
    endianness))

(define (port->bitstream port width endianness)
  (unless (= 0 (mod width 8))
    (assertion-violation 'port->bitstream
                         "width should be multiple of 8(octet)"
                         width))
  (make bitstream
        (live? #t)
        (port port)
        (width width)
        (width/octet (/ width 8))
        (read-count 0)
        (read-buffer #f)
        (write-buffer '())
        (read-pointer 0)
        (write-pointer 0)
        (endianness endianness)))

;(define* (do-bitstream-read (bs bitstream) read-width) ;; integer/eof-object
(define (do-bitstream-read bs read-width) ;; integer/eof-object

  (define (read-itr pos cur rest)
    (define (next w r)
      (let ((val (if (eof-object? r) 0 r)))
        (read-itr
          (+ pos w)
          (+ cur (bitwise-arithmetic-shift-left val pos))
          (- rest w))))
    (if (= rest 0)
      cur
      (let-with bs (width)
        (if (< width rest)
          (next width (do-bitstream-read bs width))
          (next rest (do-bitstream-read bs rest))))))

  (define (fill-buffer)
    (let-with bs (port width width/octet endianness read-count)
      (let* ((bv (make-bytevector width/octet 0))
             (r (get-bytevector-n! port bv 0 width/octet)))
        (if (eof-object? r)
          (touch! bs (live? #f)) ; finish bitstream
          (touch! bs
            (read-count (+ read-count width))
            (read-buffer (bytevector-uint-ref bv 0 endianness width/octet))
            (read-pointer 0))))))
  (let-with bs (read-buffer) (unless read-buffer (fill-buffer)))
  (let-with bs (width read-buffer read-pointer live?)
    (define available (- width read-pointer))
    (cond
      ((not live?) (eof-object))
      ((<= read-width available) ;; read from buffer
       (let* ((next (+ read-pointer read-width))
              (r (bitwise-bit-field read-buffer read-pointer next)))
         ;(display (list 'buffer-read read-pointer '=> next))(newline)
         (touch! bs (read-pointer next))
         r))
      (else ;; 
        (touch! bs 
          (read-buffer #f)
          (read-pointer width))
        (if (= available 0) ;; short-circuit
          (read-itr 0 0 read-width)
          (read-itr 
            available ;; position
            (bitwise-bit-field read-buffer read-pointer width)
            (- read-width available)))))))

;(define* (bitstream-read (bs bitstream) read-width) ;; integer/eof-object
(define (bitstream-read bs read-width) ;; integer/eof-object
  (let ((r (do-bitstream-read bs read-width)))
    ;(display (list 'bs-read read-width r))(newline)
    r))

(define* (bitstream-skip-alignment (bs bitstream) pad-width)
  (let-with bs (width read-pointer read-count)
    (define read-bits (- read-count (- width read-pointer)))
    (define skipbits (- pad-width (mod read-bits pad-width)))
    ;(display (list 'SKIP skipbits pad-width))(newline)
    (if (= pad-width skipbits)
      0
      (bitstream-read bs skipbits))))

)

