; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;;; This library provides a number of functions that reads a string from a text file,
;;;; and functions which write a string to a text file.
;;;; In particular the library supports very useful functions that read selected parts of a text file to strings.<p>
;;;; The main functions are: read-text-file and write-text-file.
;;;; The selective reading functions are read-text-file-between-marks and read-text-file-including-marks.
;;;; .title Reference Manual of the Text File Reading and Writing Library

; -----------------------------------------------------------------------------

;;; Reading stuff. 

(define (read-text-file file-name)
  ;; Return the textual contents of file-name, as a string
 (let* ((ip (open-input-file file-name))
        (res (read-text-file-from-input-port ip)))
     (close-input-port ip)
     res))

(define file-chunck 1000)

(define (read-text-file-from-input-port input-port)
  ;; Return the textual contents of input-port, as a string
 (let* ((res (read-text-file-portion input-port file-chunck)))
  (if (not (eof-object? (peek-char input-port)))
      (string-append res (read-text-file-from-input-port input-port))
      res)))

(define (read-text-file-portion input-port portion)
  ; read up to portion characters from input-port
 (let ((str (make-string portion #\space)))
  (read-into-string input-port str 0 portion)))

(define (read-into-string input-port str position max)
  ; read up to max characters into str in a tail recursive fashion.
  ; return the string
 (if (= position max)
     str
     (let ((ch (read-char input-port)))
       (cond ((eof-object? ch) (substring str 0 position))
	      (else (begin
	              (string-set! str position ch)
	              (read-into-string input-port str (+ position 1) max)))))))

; -----------------------------------------------------------------------------
; File reading between marks.

(define state-list '()) ; for debugging purposes
(define debugging-with-marks #f)

(define (read-text-file-between-marks file-name mark)
  ;; Reads the part of the file between - but excluding - two occurences of mark, and return it as a text string. 
  ;; Specifically, it read and return the string from, but excluding, the first occurence of mark
  ;; to, but excluding, the next occurence of mark.
  ;; If no occurence of mark can be located, return the empty string.
  ;; If only one occurence of mark occurs, return a suffix of the file from the mark to the end of file.
  ;; This function is implemented by means of a state machine which controls when the actual reading must take place.
 (set! current-state 'skip)
 (if debugging-with-marks (set! state-list (list 'skip)))
 (let* ((ip (open-input-file file-name))
        (res (read-text-file-from-input-port-with-marks ip mark)))
     (close-input-port ip)
     (set! current-state 'skip)
     res))

(define file-chunck-with-marks 1000)
(define current-state 'skip)

(define (read-text-file-from-input-port-with-marks input-port mark)
  ; return the textual contents of file-name, as a string
 (let* ((res (read-text-file-portion-with-marks input-port file-chunck-with-marks mark)))
  (if (not (eof-object? (peek-char input-port)))
      (string-append res (read-text-file-from-input-port-with-marks input-port mark))
      res)))

(define (read-text-file-portion-with-marks input-port portion mark)
  ; read up to portion characters from input-port
 (let ((str (make-string portion #\space)))
  (read-into-string-with-marks input-port str 0 portion mark)))

(define (read-into-string-with-marks input-port str position max mark)
; read up to max characters into str in a tail recursive fashion. Return the string.
  (if (= position max)
      str
      (let ((ch (read-char input-port)))
	(cond ((eof-object? ch) (substring str 0 position))
	      (else (let* ((trans-result (transition current-state ch mark))
			   (next-state (car trans-result))
			   (output-function (cdr trans-result))
			   (output-string (output-function current-state next-state ch mark)))
                      (if debugging-with-marks
                          (set! state-list (cons next-state state-list)))
		      (put-into-string! str position output-string)
                      (set! current-state next-state)
		      (read-into-string-with-marks input-port str (+ position (string-length output-string)) max mark)))))))

(define (put-into-string! str position addition)
  (if (> (string-length addition) 0)
      (begin (string-set! str position (string-ref addition 0))
             (put-into-string! str (+ 1 position) (substring addition 1 (string-length addition))))))

; -------------------------------------------------
; The state machine controlling the reading process between marks
; The marks are not included in the text returned

; States are either a symbol or a number:
;   The symbol skip: Do not collect the character read.
;   The symbol skip-rest: Reading is done. Never collect more characters.
;                         (prevents re-reading after marked region).           
;   The symbol collect: Do collect the character read.
;   A positive number n: We have recognized n characters of the mark
;                        from a skipping state towards a collecting state
;   A negative number n: We have recognized -n characters of the mark
;                        from collecting state towards a skipping state.
; 
; Observation: In negatively numbered states, we have to output a partially
; matched mark. This is done by output-pending-mark.


(define (output-skip in-state out-state char mark)
  "")

(define (output-let-go in-state out-state char mark) 
  (as-string char))

(define (output-pending-mark in-state out-state char mark)
  ; output the portion of the mark already read
  (substring mark 0 (abs in-state)))


(define (transition in-state char mark)
  (let ((mark0 (string-ref mark 0)))  ; first char in mark
    (cond  ((and (symbol? in-state) (eq? in-state 'skip))
              (cond ((eqv? char mark0)                            (cons 1 output-skip))
                    (else                                        (cons 'skip output-skip))))

           ((and (symbol? in-state) (eq? in-state 'collect))     
              (cond ((eqv? char mark0)                            (cons -1 output-skip))
                    (else                                        (cons 'collect output-let-go))))

           ((and (positive-number? in-state) (< in-state (string-length mark))
              (cond ((eqv? char (string-ref mark in-state))       
                                                                 (cons (+ in-state 1) output-skip))
                    (else                                        (cons 'skip output-skip)))))

           ((and (positive-number? in-state) (= in-state (string-length mark)))
                                                                 (cons 'collect output-let-go))

           ((and (negative-number? in-state) (< (abs in-state) (string-length mark)))
              (cond ((eqv? char (string-ref mark (abs in-state)))       
                                                                 (cons (- in-state 1) output-skip))
                    (else                                        (cons 'collect output-pending-mark))))

           ((and (negative-number? in-state) (= (abs in-state) (string-length mark)))
                                                                 (cons 'skip-rest output-skip))

           ((and (symbol? in-state) (eq? in-state 'skip-rest))   (cons 'skip-rest output-skip))

       )))

(define (positive-number? x)
  (and (number? x) (> x 0)))


(define (negative-number? x)
  (and (number? x) (< x 0)))



; -----------------------------------------------------------------------------
; File reading including start-mark and end-mark
; We support potential different start and end marks

(define (read-text-file-including-marks file-name start-mark end-mark)
  ;; Reads the part of the file between and including occurences of start-mark and end-mark, and return it as a text string.
  ;; Specifically, read and return the string from and including the first occurence of start-mark
  ;; to and including the next occurence of end-mark. 
  ;; If no occurence of the mark can be located, return the empty string.
  ;; If only the start-mark occurs, return a suffix of the file from the start-mark to the end of file.
  ;; This function is implemented by means of a state machine which controls when the actual reading must take place.
 (set! current-state 'skip)
 (if debugging-with-marks (set! state-list (list 'skip)))
 (let* ((ip (open-input-file file-name))
        (res (read-text-file-from-input-port-including-marks ip start-mark end-mark)))
     (close-input-port ip)
     (set! current-state 'skip)
     res))

(define (read-text-file-from-input-port-including-marks input-port start-mark end-mark)
  ; return the textual contents of file-name, as a string
 (let* ((res (read-text-file-portion-including-marks input-port file-chunck-with-marks start-mark end-mark)))
  (if (not (eof-object? (peek-char input-port)))
      (string-append res (read-text-file-from-input-port-including-marks input-port start-mark end-mark))
      res)))

(define (read-text-file-portion-including-marks input-port portion start-mark end-mark)
  ; read up to portion characters from input-port
 (let ((str (make-string portion #\space)))
  (read-into-string-including-marks input-port str 0 portion start-mark end-mark)))

(define (read-into-string-including-marks input-port str position max start-mark end-mark)
; read up to max characters into str in a tail recursive fashion. Return the string.
  (if (= position max)
      str
      (let ((ch (read-char input-port)))
	(cond ((eof-object? ch) (substring str 0 position))
	      (else (let* ((trans-result (transition1 current-state ch start-mark end-mark))
			   (next-state (car trans-result))
			   (output-function (cdr trans-result))
			   (output-string (output-function current-state next-state ch start-mark end-mark)))
                      (if debugging-with-marks
                          (set! state-list (cons next-state state-list)))
		      (put-into-string! str position output-string)
                      (set! current-state next-state)
		      (read-into-string-including-marks input-port str (+ position (string-length output-string))
                                                        max start-mark end-mark)))))))

(define (put-into-string! str position addition)
  (if (> (string-length addition) 0)
      (begin (string-set! str position (string-ref addition 0))
             (put-into-string! str (+ 1 position) (substring addition 1 (string-length addition))))))

; -------------------------------------------------
; The state machine controlling the reading process between marks
; The marks are not included in the text returned

; States are either a symbol or a number:
;   The symbol skip: Do not collect the character read.
;   The symbol skip-rest: Reading is done. Never collect more characters.
;                         (prevents re-reading after marked region).           
;   The symbol collect: Do collect the character read.
;   A positive number n: We have recognized n characters of the start mark,
;                        from a skipping state towards a collecting state
;   A negative number n: We have recognized -n characters of the end-mark, 
;                        from collecting state towards a skipping state.
; 
; Observation: In negatively numbered states, we have to output a partially
; matched mark. This is done by output-pending-mark.


(define (output-skip1 in-state out-state char start-mark end-mark)
  "")

(define (output-let-go1 in-state out-state char start-mark end-mark) 
  (as-string char))


(define (output-pending-start-mark in-state out-state char start-mark end-mark)
  ; output the portion of the mark already read
  (string-append start-mark (as-string char)))

(define (output-pending-end-mark in-state out-state char start-mark end-mark)
  ; output the portion of the mark already read
  (substring end-mark 0 (abs in-state)))


(define (transition1 in-state char start-mark end-mark)
  (let ((mark0 (string-ref start-mark 0))  ; first char in start-mark
        (markn (string-ref end-mark 0))    ; first char in end-mark
       )
    (cond  ((and (symbol? in-state) (eq? in-state 'skip))
              (cond ((eqv? char mark0)                            (cons 1 output-skip1))
                    (else                                        (cons 'skip output-skip1))))

           ((and (symbol? in-state) (eq? in-state 'collect))     
              (cond ((eqv? char markn)                            (cons -1 output-let-go1))
                    (else                                        (cons 'collect output-let-go1))))

           ((and (positive-number? in-state) (< in-state (string-length start-mark))
              (cond ((eqv? char (string-ref start-mark in-state))       
                                                                 (cons (+ in-state 1) output-skip1))
                    (else                                        (cons 'skip output-skip1)))))

           ((and (positive-number? in-state) (= in-state (string-length start-mark)))
                                                                 (cons 'collect output-pending-start-mark))

           ((and (negative-number? in-state) (< (abs in-state) (string-length end-mark)))
              (cond ((eqv? char (string-ref end-mark (abs in-state)))       
                                                                 (cons (- in-state 1) output-let-go1))
                    (else                                        (cons 'collect output-let-go1))))

           ((and (negative-number? in-state) (= (abs in-state) (string-length end-mark)))
                                                                 (cons 'skip-rest output-skip1))

           ((and (symbol? in-state) (eq? in-state 'skip-rest))   (cons 'skip-rest output-skip1))

       )))


; ---------------------------------------------------------------------------------------------------------------

;; Convenient and specialized function returning the contents of a program file.
;; Other usages are also possible.
;; Reads an external file, either an entire file (if no optional mark parameter)
;; or a region surrounded by mark.

(define (code-example file . mark)
  (let ((code-text
           (if (null? mark) 
               (read-text-file file)
               (read-text-file-between-marks file (car mark)))))
     code-text))





; ---------------------------------------------------------------------------------------------------------------

;;; Writing stuff. 

;; Write the text string str to the file named file-name. 
;; After this function call, the file contains exactly the string str.
;; Opening and closing is done by this function.
;; .form (write-text-file str file-name [suppress-cr])
;; .parameter suppress-cr If true, never write a cr (character 13). Ie. never use PC end of line conventions.
(define (write-text-file str file-name . optional-parameter-list)
 (let ((suppress-cr (optional-parameter 1 optional-parameter-list #f)))
   (if (file-exists? file-name) (delete-file file-name))
   (let* ((op (open-output-file file-name)))
     (write-text-file-to-port str op suppress-cr)
     (close-output-port op))))

;; Write the string str to port, which is assumed to be open.
;; This procedure does not close the port.
;; .form (write-string-to-port str port [suppress-cr])
;; .parameter suppress-cr If true, never write a cr (character 13). Ie. never use PC end of line conventions.
;; .internal-references "similar function" "write-port-strings"
(define (write-string-to-port str port . optional-parameter-list)
 (let ((suppress-cr (optional-parameter 1 optional-parameter-list #f)))
  (write-text-file-to-port str port suppress-cr)))

;; Write each of the strings in the parameter strings to port.
;; The output port port is assumed to be open.
;; This procedure does not close the port.
;; .parameter port An open output port
;; .parameter strings A list of strings (a rest parameter of the function).
;; .internal-references "similar function" "write-string-to-port"
;; .misc Notice the order of the parameters compared with write-string-to-port.
(define (write-port-strings port . strings)
  (for-each
     (lambda (str) (write-string-to-port str port))
     strings))

(define (write-text-file-to-port str port suppress-cr)
  (write-text-file-to-port-1 0 (string-length str) str port suppress-cr))

(define (write-text-file-to-port-1 i max str port suppress-cr)
  (if (< i max)
    (begin
      (let ((ch (string-ref str i)))
       (if suppress-cr
           (if (not (eqv? ch #\return)) (write-char ch port))
           (write-char ch port))
       (write-text-file-to-port-1 (+ i 1) max str port suppress-cr)))))
  

