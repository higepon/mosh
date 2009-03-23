(define (string-titlecase2 s)
  (let ((input (open-string-input-port s)))
    (receive (output get-string) (open-string-output-port)
      (letrec ((titlecase-first-char
                (lambda ()
                  (let loop ((ch (read-char input)))
                    (cond ((eof-object? ch)
                           (get-string))
                          (else
                           (case (char-general-category ch)
                             ((Ll Lu Lt)
                              (display (char-titlecase ch) output)
                              (downcase-subsequence))
                             (else
                              (display ch output)
                              (loop (read-char input)))))))))
               (downcase-subsequence
                (lambda ()
                  (let loop ((ch (read-char input)))
                    (cond ((eof-object? ch)
                           (get-string))
                          (else
                           (case (char-general-category ch)
                             ((Ll Lu Lt)
                              (display (char-downcase ch) output)
                              (format #t "ch downcase ~a => ~a\n" ch (char-downcase ch))
                              (loop (read-char input)))
                             ((Po Pf)
                              (case ch
                                ((#\x0027          ; MidLetter # Po       APOSTROPHE
                                  #\x003A          ; MidLetter # Po       COLON
                                  #\x00B7          ; MidLetter # Po       MIDDLE DOT
                                  #\x05F4          ; MidLetter # Po       HEBREW PUNCTUATION GERSHAYIM
                                  #\x2019          ; MidLetter # Pf       RIGHT SINGLE QUOTATION MARK
                                  #\x2027          ; MidLetter # Po       HYPHENATION POINT
                                  )
                                 (display ch output)
                                 (loop (read-char input)))
                                (else
                                 (display ch output)
                                 (titlecase-first-char))))
                             ((Nd)
                              (display ch output)
                              (loop (get-char input)))
                             (else
                              (display ch output)
                              (titlecase-first-char)))))))))
        (let ((new (titlecase-first-char)))
          (if (string=? s new) s new))))))

;; (display "****")
(display (string-titlecase2 "R6RS"))

;; (display (char-titlecase #\r))
;; (display (char-titlecase #\R))
;; (display (char-titlecase #\s))
;; (display (char-titlecase #\6))
