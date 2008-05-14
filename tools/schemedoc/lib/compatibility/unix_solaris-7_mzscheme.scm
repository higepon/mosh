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

;;;; MzScheme specific stuff to be loaded for compatibility. 
;;;; This file implements each of the necessary non-R4RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in MzScheme.<p>
;;;; Notice that similar files exists for all other Scheme systems, platforms and operating systems on which LAML is running.
;;;; The files are located in the directory lib/compatibility/.
;;;; .title MzScheme LAML Compatibility 


(require-library "compat.ss")

(require-library "synrule.ss")

;;; Definition of non-R4RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R4RS).

;; Return the current time in seconds
(define (current-time)
  (current-seconds))

; ---------------------------------------------------------------------------------------------------
; file-exists?, delete-file, copy-file and directory-exists? all exist in MzScheme

; ---------------------------------------------------------------------------------------------------

;; Make a new directory, new-dir, in the directory path (first parameter).
;; The parameter in-directory-path ends in a slash.
(define (make-directory-in-directory in-directory-path new-dir)
  (make-directory (string-append in-directory-path new-dir)))

; ---------------------------------------------------------------------------------------------------


;; Mail sending support: Send an email to a receiver with title and contents.
;; The optional parameter temp-dir gives a temporary directory used for the mail sending; default is "temp/"
(define (mail receiver title contents . temp-dir)
 (let ((temp-dir-1 (if (null? temp-dir) "temp/" (car temp-dir))))
  (write-text-file contents (string-append temp-dir-1 "temp-mailfile"))
  (system
    (string-append
        "/usr/ucb/mail -s " "'" title "' " receiver (string-append "< " temp-dir-1 "temp-mailfile")))))

; -----------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.


;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght four or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (witout extension and initial path).
;; Element number three must be a slash terminated directory, in the source file resides.
;; Element number four must be a list of program parameters.
;; This function must be redefined in scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (if (and (vector? argv) (>= (vector-length argv) 2))
        (list 'laml 
               (file-name-proper (vector-ref argv 0)) (vector-ref argv 1) 
               (if (>= (vector-length argv) 3) (vector-ref argv 2) '()))
      #f))

;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; Source-file must be a file name without initial path and without extension.
;; start-dir must be an absolute path to a directory ending in a slash /.
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in scheme-system dependent compatibility file
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (let ((program-parameters (optional-parameter 1 optional-parameter-list '()))
        (a (make-vector 3 #f)))
    (vector-set! a 0 source-file)
    (vector-set! a 1 startup-dir)
    (vector-set! a 2 program-parameters)
    (set! argv a)))
  

(error-print-width 1000)


;; Sort list using the comparison predicate, using MzScheme native sort function.
(define (sort-list list com)
  (if (null? list) list (sort com list)))

(define eval-cur-env eval)

; Case sensitive reading
(read-case-sensitive #t)