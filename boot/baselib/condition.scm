; condition.scm - condition
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

(define &condition
  (let* ((rtd (make-record-type-descriptor '&condition #f '&conidion-uid #f #f '#()))
         (rcd (make-record-constructor-descriptor rtd #f #f)))
    (make-record-type '&condition rtd rcd)))

(define compound-condition-component (lambda (obj) (tuple-ref obj 1)))

(define condition
  (lambda components
    (tuple 'type:condition
           (apply append
                  (map (lambda (component)
                         (or (condition? component)
                             (assertion-violation 'condition (format "expected condition, but got ~a" component) components))
                         (simple-conditions component))
                       components)))))

(define compound-condition?
  (lambda (obj)
    (and (tuple? obj)
         (eq? 'type:condition (tuple-ref obj 0)))))

(define simple-condition?
  (lambda (obj)
    (and (record? obj)
         (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj)))))

(define condition?
  (lambda (obj)
    (or (simple-condition? obj)
        (compound-condition? obj))))

(define simple-conditions
  (lambda (c)
    (cond ((simple-condition? c) (list c))
          ((compound-condition? c) (compound-condition-component c))
          (else
           (assertion-violation 'simple-conditions (format "expected condition, but got ~a" c))))))

(define any1
  (lambda (pred lst)
    (and (not (null? lst))
         (or (pred (car lst)) (any1 pred (cdr lst))))))
(define condition-predicate
  (lambda (rtd)
    (or (rtd-ancestor? (record-type-rtd &condition) rtd)
        (assertion-violation 'condition-predicate (format "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd)))
    (lambda (obj)
      (cond ((simple-condition? obj)
             (rtd-ancestor? rtd (record-rtd obj)))
            ((compound-condition? obj)
             (any1 (lambda (component) (rtd-ancestor? rtd (record-rtd component)))
                   (compound-condition-component obj)))
            (else #f)))))

(define condition-accessor
  (lambda (rtd proc)

    (define wrong-type
      (lambda (rtd obj)
        (assertion-violation "condition accessor" (format "expected condition of a subtype of ~s, but got ~a" rtd obj) rtd obj)))

    (or (rtd-ancestor? (record-type-rtd &condition) rtd)
        (assertion-violation 'condition-accessor (format "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd) rtd proc))

    (lambda (obj)
      (cond ((simple-condition? obj)
             (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj))
             (proc obj))
            ((compound-condition? obj)
             (cond ((any1 (lambda (component) (and (rtd-ancestor? rtd (record-rtd component)) component))
                          (compound-condition-component obj))
                    => proc)
                   (else
                    (wrong-type rtd obj))))
            (else
             (wrong-type rtd obj))))))

(define describe-condition
  (lambda (port c)

    (define list-parents
      (lambda (rtd)
        (let loop ((rtd rtd) (lst '()))
          (cond ((record-type-parent rtd)
                 => (lambda (p) (loop p (cons (record-type-name p) lst))))
                (else (reverse (cdr lst)))))))

    (cond ((condition? c)
           (let ((buf (make-string-output-port)))
             (format buf "  #<condition~!")
             (let ((lst (simple-conditions c)))
               (for-each (lambda (rec)
                           (let ((rtd (record-rtd rec)))
                             (let ((name (record-type-name rtd))
                                   (parents (list-parents rtd))
                                   (count (vector-length (record-type-field-names rtd))))
                               (format buf "~%    ~a" name)
                               (and (pair? parents) (format buf " ~a" parents))
                               (cond ((= count 1)
                                      (let ((obj ((record-accessor rtd 0) rec)))
                                        (if (string? obj)
                                            (format buf ": ~a" ((record-accessor rtd 0) rec))
                                            (format buf ": ~a" ((record-accessor rtd 0) rec)))))
                                     ((> count 1)
                                      (let ((lst (vector->list (record-type-field-names rtd))))
                                        (let loop ((i 0) (lst lst))
                                          (and (pair? lst)
                                               (let ((obj ((record-accessor rtd i) rec)))
                                                 (if (string? obj)
                                                     (format buf "~%     ~a: ~a" (car lst) obj)
                                                     (format buf "~%     ~a: ~a" (car lst) obj))
                                                 (loop (+ i 1) (cdr lst)))))))))))
                         lst))
             (format buf "~%   >")
             (format port "~a~!" (extract-accumulated-string buf))))
          (else
           (format port "~a~!" c)))))

(define &message
  (let ((rtd (make-record-type-descriptor '&message (record-type-rtd &condition) '&message-uid #f #f '#((immutable message)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&message rtd rcd))))
(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))

(define &warning
  (let ((rtd (make-record-type-descriptor '&warning (record-type-rtd &condition) '&warning-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&warning rtd rcd))))
(define make-warning (record-constructor (record-type-rcd &warning)))
(define warning? (condition-predicate (record-type-rtd &warning)))

(define &serious
  (let ((rtd (make-record-type-descriptor '&serious (record-type-rtd &condition) '&serious-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&serious rtd rcd))))
(define make-serious-condition (record-constructor (record-type-rcd &serious)))
(define serious-condition? (condition-predicate (record-type-rtd &serious)))

(define &error
  (let ((rtd (make-record-type-descriptor '&error (record-type-rtd &serious) '&error-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &serious) #f)))
      (make-record-type '&error rtd rcd))))
(define make-error (record-constructor (record-type-rcd &error)))
(define error? (condition-predicate (record-type-rtd &error)))

(define &violation
  (let ((rtd (make-record-type-descriptor '&violation (record-type-rtd &serious) '&violation-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &serious) #f)))
      (make-record-type '&violation rtd rcd))))
(define make-violation (record-constructor (record-type-rcd &violation)))
(define violation? (condition-predicate (record-type-rtd &violation)))

(define &assertion
  (let ((rtd (make-record-type-descriptor '&assertion (record-type-rtd &violation) '&assertion-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&assertion rtd rcd))))
(define make-assertion-violation (record-constructor (record-type-rcd &assertion)))
(define assertion-violation? (condition-predicate (record-type-rtd &assertion)))

(define &irritants
  (let ((rtd (make-record-type-descriptor '&irritants (record-type-rtd &condition) '&irritants-uid #f #f '#((immutable irritants)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&irritants rtd rcd))))
(define &irritants-irritants (record-accessor (record-type-rtd &irritants) 0))
(define make-irritants-condition (record-constructor (record-type-rcd &irritants)))
(define irritants-condition? (condition-predicate (record-type-rtd &irritants)))
(define condition-irritants (condition-accessor (record-type-rtd &irritants) &irritants-irritants))

(define &who
  (let ((rtd (make-record-type-descriptor '&who (record-type-rtd &condition) '&who-uid #f #f '#((immutable who)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&who rtd rcd))))
(define &who-who (record-accessor (record-type-rtd &who) 0))
(define make-who-condition (record-constructor (record-type-rcd &who)))
(define who-condition? (condition-predicate (record-type-rtd &who)))
(define condition-who (condition-accessor (record-type-rtd &who) &who-who))

(define &non-continuable
  (let ((rtd (make-record-type-descriptor '&non-continuable (record-type-rtd &violation) '&non-continuable-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&non-continuable rtd rcd))))
(define make-non-continuable-violation (record-constructor (record-type-rcd &non-continuable)))
(define non-continuable-violation? (condition-predicate (record-type-rtd &non-continuable)))

(define &implementation-restriction
  (let ((rtd (make-record-type-descriptor '&implementation-restriction (record-type-rtd &violation) '&implementation-restriction-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&implementation-restriction rtd rcd))))
(define make-implementation-restriction-violation
  (record-constructor (record-type-rcd &implementation-restriction)))
(define implementation-restriction-violation?
  (condition-predicate (record-type-rtd &implementation-restriction)))

(define &lexical
  (let ((rtd (make-record-type-descriptor '&lexical (record-type-rtd &violation) '&lexical-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&lexical rtd rcd))))
(define make-lexical-violation (record-constructor (record-type-rcd &lexical)))
(define lexical-violation? (condition-predicate (record-type-rtd &lexical)))

(define &syntax
  (let ((rtd (make-record-type-descriptor '&syntax (record-type-rtd &violation) '&syntax-uid #f #f '#((immutable form) (immutable subform)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&syntax rtd rcd))))
(define &syntax-form (record-accessor (record-type-rtd &syntax) 0))
(define &syntax-subform (record-accessor (record-type-rtd &syntax) 1))
(define make-syntax-violation (record-constructor (record-type-rcd &syntax)))
(define syntax-violation? (condition-predicate (record-type-rtd &syntax)))
(define syntax-violation-form (condition-accessor (record-type-rtd &syntax) &syntax-form))
(define syntax-violation-subform (condition-accessor (record-type-rtd &syntax) &syntax-subform))

(define &undefined
  (let ((rtd (make-record-type-descriptor '&undefined (record-type-rtd &violation) '&undefind-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&undefined rtd rcd))))
(define make-undefined-violation (record-constructor (record-type-rcd &undefined)))
(define undefined-violation? (condition-predicate (record-type-rtd &undefined)))

(define &i/o
  (let ((rtd (make-record-type-descriptor '&i/o (record-type-rtd &error) '&i/o-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &error) #f)))
      (make-record-type '&i/o rtd rcd))))
(define make-i/o-error (record-constructor (record-type-rcd &i/o)))
(define i/o-error? (condition-predicate (record-type-rtd &i/o)))

(define &i/o-read
  (let ((rtd (make-record-type-descriptor '&i/o-read (record-type-rtd &i/o) '&i/o-read-uid  #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-read rtd rcd))))
(define make-i/o-read-error (record-constructor (record-type-rcd &i/o-read)))
(define i/o-read-error? (condition-predicate (record-type-rtd &i/o-read)))

(define &i/o-write
  (let ((rtd (make-record-type-descriptor '&i/o-write (record-type-rtd &i/o) '&i/o-write-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-write rtd rcd))))
(define make-i/o-write-error (record-constructor (record-type-rcd &i/o-write)))
(define i/o-write-error? (condition-predicate (record-type-rtd &i/o-write)))

(define &i/o-invalid-position
  (let ((rtd (make-record-type-descriptor '&i/o-invalid-position (record-type-rtd &i/o) '&i/o-invalid-position-uid #f  #f '#((immutable position)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-invalid-position rtd rcd))))
(define &i/o-invalid-position-position
  (record-accessor (record-type-rtd &i/o-invalid-position) 0))
(define make-i/o-invalid-position-error
  (record-constructor (record-type-rcd &i/o-invalid-position)))
(define i/o-invalid-position-error? (condition-predicate (record-type-rtd &i/o-invalid-position)))
(define i/o-error-position
  (condition-accessor (record-type-rtd &i/o-invalid-position) &i/o-invalid-position-position))

(define &i/o-filename
  (let ((rtd (make-record-type-descriptor '&i/o-filename (record-type-rtd &i/o) '&i/o-filename-ui #f #f '#((immutable filename)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-filename rtd rcd))))
(define &i/o-filename-filename (record-accessor (record-type-rtd &i/o-filename) 0))
(define make-i/o-filename-error (record-constructor (record-type-rcd &i/o-filename)))
(define i/o-filename-error? (condition-predicate (record-type-rtd &i/o-filename)))
(define i/o-error-filename (condition-accessor (record-type-rtd &i/o-filename) &i/o-filename-filename))

(define &i/o-file-protection
  (let ((rtd (make-record-type-descriptor '&i/o-file-protection (record-type-rtd &i/o-filename) '&i/o-file-protection-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-protection rtd rcd))))
(define make-i/o-file-protection-error
  (record-constructor (record-type-rcd &i/o-file-protection)))
(define i/o-file-protection-error? (condition-predicate (record-type-rtd &i/o-file-protection)))

(define &i/o-file-is-read-only
  (let ((rtd (make-record-type-descriptor '&i/o-file-is-read-only (record-type-rtd &i/o-file-protection) '&i/o-file-is-read-only-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-file-protection) #f)))
      (make-record-type '&i/o-file-is-read-only rtd rcd))))
(define make-i/o-file-is-read-only-error (record-constructor (record-type-rcd &i/o-file-is-read-only)))
(define i/o-file-is-read-only-error? (condition-predicate (record-type-rtd &i/o-file-is-read-only)))

(define &i/o-file-already-exists
  (let ((rtd (make-record-type-descriptor '&i/o-file-already-exists (record-type-rtd &i/o-filename) '&i/o-file-already-exists-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-already-exists rtd rcd))))
(define make-i/o-file-already-exists-error (record-constructor (record-type-rcd &i/o-file-already-exists)))
(define i/o-file-already-exists-error? (condition-predicate (record-type-rtd &i/o-file-already-exists)))

(define &i/o-file-does-not-exist
  (let ((rtd (make-record-type-descriptor '&i/o-file-does-not-exist (record-type-rtd &i/o-filename) '&i/o-file-does-not-exist-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-does-not-exist rtd rcd))))
(define make-i/o-file-does-not-exist-error (record-constructor (record-type-rcd &i/o-file-does-not-exist)))
(define i/o-file-does-not-exist-error? (condition-predicate (record-type-rtd &i/o-file-does-not-exist)))

(define &i/o-port
  (let ((rtd (make-record-type-descriptor '&i/o-port (record-type-rtd &i/o) '&i/o-port-uid #f #f '#((immutable port)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-port rtd rcd))))
(define &i/o-port-port (record-accessor (record-type-rtd &i/o-port) 0))
(define make-i/o-port-error (record-constructor (record-type-rcd &i/o-port)))
(define i/o-port-error? (condition-predicate (record-type-rtd &i/o-port)))
(define i/o-error-port (condition-accessor (record-type-rtd &i/o-port) &i/o-port-port))

(define &i/o-decoding
  (let ((rtd (make-record-type-descriptor '&i/o-decoding (record-type-rtd &i/o-port) '&i/o-decoding-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-port) #f)))
      (make-record-type '&i/o-decoding rtd rcd))))
(define make-i/o-decoding-error (record-constructor (record-type-rcd &i/o-decoding)))
(define i/o-decoding-error? (condition-predicate (record-type-rtd &i/o-decoding)))

(define &i/o-encoding
  (let ((rtd (make-record-type-descriptor '&i/o-encoding (record-type-rtd &i/o-port) '&i/o-encoding #f #f '#((immutable char)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-port) #f)))
      (make-record-type '&i/o-encoding rtd rcd))))
(define &i/o-encoding-char (record-accessor (record-type-rtd &i/o-encoding) 0))
(define make-i/o-encoding-error (record-constructor (record-type-rcd &i/o-encoding)))
(define i/o-encoding-error? (condition-predicate (record-type-rtd &i/o-encoding)))
(define i/o-encoding-error-char (condition-accessor (record-type-rtd &i/o-encoding) &i/o-encoding-char))

(define &no-infinities
  (let ((rtd (make-record-type-descriptor '&no-infinities (record-type-rtd &implementation-restriction) '&no-infinities-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &implementation-restriction) #f)))
      (make-record-type '&no-infinities rtd rcd))))
(define make-no-infinities-violation (record-constructor (record-type-rcd &no-infinities)))
(define no-infinities-violation? (condition-predicate (record-type-rtd &no-infinities)))

(define &no-nans
  (let ((rtd (make-record-type-descriptor '&no-nans (record-type-rtd &implementation-restriction) '&no-nans-uid #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &implementation-restriction) #f)))
      (make-record-type '&no-nans rtd rcd))))
(define make-no-nans-violation (record-constructor (record-type-rcd &no-nans)))
(define no-nans-violation? (condition-predicate (record-type-rtd &no-nans)))
