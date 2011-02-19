#!r6rs

(library (tests r6rs io simple)
  (export run-io-simple-tests)
  (import (rnrs)
          (mosh)
          (tests r6rs test))

  (define tmp-file (if (string=? "mona" (host-os)) "/MEM/io-tmp2" "io-tmp2"))

  (define (run-io-simple-tests)

    (test/unspec
     (when (file-exists? tmp-file)
       (delete-file tmp-file)))

    (test/values (call-with-output-file tmp-file
                   (lambda (p)
                     (test (output-port? p) #t)
                     (test (binary-port? p) #f)
                     (test (textual-port? p) #t)
                     (test/unspec (write-char #\q p))
                     (test/unspec (newline p))
                     (test/unspec (display "more" p))
                     (test/unspec (write "last" p))
                     (values 3 4)))
                 3 4)

    (test/values (call-with-input-file tmp-file
                   (lambda (p)
                     (test (input-port? p) #t)
                     (test (binary-port? p) #f)
                     (test (textual-port? p) #t)
                     (test (peek-char p) #\q)
                     (test (read-char p) #\q)
                     (test (read-char p) #\newline)
                     (test (read-char p) #\m)
                     (test (read-char p) #\o)
                     (test (peek-char p) #\r)
                     (test (read-char p) #\r)
                     (test (read-char p) #\e)
                     (test (read p) "last")
                     (test (read p) (eof-object))
                     (values 7 8 9)))
                 7 8 9)

    (test/unspec (delete-file tmp-file))

    (let ([p (open-output-file tmp-file)])
      (test (output-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test/unspec (write-char #\! p))
      (test/unspec (close-output-port p)))

    (let ([p (open-input-file tmp-file)])
      (test (input-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test (read-char p) #\!)
      (test/unspec (close-input-port p)))

    (test/unspec (delete-file tmp-file))

    (test/values (with-output-to-file tmp-file
                   (lambda ()
                     (test/unspec (write-char #\z))
                     (test/unspec (newline))
                     (test/unspec (display "a"))
                     (test/unspec (write "a"))
                     (values 30 40)))
                 30 40)

    (test/values (with-input-from-file tmp-file
                   (lambda ()
                     (test (peek-char) #\z)
                     (test (read-char) #\z)
                     (test (read) 'a)
                     (test (read) "a")
                     (test (read) (eof-object))
                     (values 70 80 90)))
                 70 80 90)

    (test/unspec
     (when (file-exists? tmp-file)
       (delete-file tmp-file)))
    
    (test (input-port? (current-input-port)) #t)
    (test (binary-port? (current-input-port)) #f)
    (test (textual-port? (current-input-port)) #t)

    (test (output-port? (current-output-port)) #t)
    (test (binary-port? (current-output-port)) #f)
    (test (textual-port? (current-output-port)) #t)

    (test (output-port? (current-error-port)) #t)
    (test (binary-port? (current-error-port)) #f)
    (test (textual-port? (current-error-port)) #t)

    ;;
    ))
