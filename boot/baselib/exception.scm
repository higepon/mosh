; exception.scm - exception
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

;; Originally from Ypsilon Scheme
(define assertion-violation
  (lambda (who message . irritants)
    (if (or (not who) (string? who) (symbol? who) (identifier? who))
        (if (string? message)
            (raise
             (apply condition
                    (filter values
                            (list (make-assertion-violation)
                                  (and who (make-who-condition who))
                                  (make-message-condition message)
                                  (make-irritants-condition irritants)))))
            (assertion-violation 'assertion-violation (wrong-type-argument-message "string" message 2)))
        (assertion-violation 'assertion-violation (wrong-type-argument-message "string, symbol, or #f" who 1)))))

(define undefined-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-undefined-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define lexical-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-lexical-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define raise-lexical-violation-read-error
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-lexical-violation)
                          (make-i/o-read-error)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (raise
     (apply condition
            (filter values
                    (list (apply constructor options)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? options)
                               (make-irritants-condition options))))))))

(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (raise
     (apply condition
            (filter values
                    (list (apply constructor options)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and port (make-i/o-port-error port))
                          (make-irritants-condition (cons* port options))))))))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))

(define implementation-restriction-violation
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-implementation-restriction-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-file-is-read-only-error who message port port)))

(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-i/o-filename-error filename)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise-misc-i/o-error-with-port make-i/o-invalid-position-error who message port position)))

(define raise-i/o-read-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-read-error who message port)))

(define raise-i/o-write-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-write-error who message port)))

(define error
  (lambda (who message . irritants)
    (if (or (not who) (string? who) (symbol? who)) ;; (identifier? who)
        (if (string? message)
            (raise
             (apply condition
                    (filter values
                            (list (make-error)
                                  (and who (make-who-condition who))
                                  (make-message-condition message)
                                  (make-irritants-condition irritants)))))
            (assertion-violation 'error (wrong-type-argument-message "string" message 2)))
        (assertion-violation 'error (wrong-type-argument-message "string, symbol, or #f" who 1)))))
