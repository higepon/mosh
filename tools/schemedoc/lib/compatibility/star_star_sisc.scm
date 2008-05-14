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


;;;; SISC specific stuff to be loaded for compatibility. 
;;;; By Matthias Radestock, matthias@lshift.net.
;;;; This file implements each of the necessary non-R4RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in SISC.

;;; Definition of non-R4RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R4RS).

; ---------------------------------------------------------------------------------------------------

(define (current-time) (quotient (system-time) 1000))

; ---------------------------------------------------------------------------------------------------

(load "sorting/sort.scm")
;; Sort list by means of the comparison function com
(define sort-list sort:sort)

; ---------------------------------------------------------------------------------------------------
(import file-manipulation)

(define delete-file file-delete!)

; ---------------------------------------------------------------------------------------------------

; Returns whether a directory exist. 
; Works both on names without and with trailing slash.
; An alias of file-exists?
(define directory-exists? file-exists?)

; ---------------------------------------------------------------------------------------------------

(import block-io) ;;older SISC
;;(import binary-io) ;;latest SISC
(define (copy-file source destination)
  (let ([buffer (make-buffer 1024)])
    (call-with-input-file source
      (lambda (input)
	(call-with-output-file destination
	  (lambda (output)
	    (let loop ()
	      (let ([count (block-read buffer input 1024)])
		(and (not (eof-object? count))
		     (begin
		       (block-write buffer output count)
		       (loop)))))))))))

; ---------------------------------------------------------------------------------------------------

; Make a new directory, new-dir, in the directory path (first parameter).
; The parameter in-directory-path ends in a slash.
; This implementation works only on UNIX systems.
(define (make-directory-in-directory in-directory-path new-dir)
  (make-directory! (string-append in-directory-path new-dir)))

; This definition is for backward compatility for cgi applications using SISC.
; I need to go through theese and change the name of the function
(define make-directory make-directory-in-directory)


; Mail sending support: Send an email to a receiver.
; This implementation works only on UNIX Systems.
(define (mail receiver title contents)
  (write-text-file contents "temp/temp-mailfile")
  (system
    (string-append
        "/usr/ucb/mail -s " "'" title "' " receiver "< temp/temp-mailfile")))

;; Return a directory list of path
;; -- supported natively

(define bound?
  (let ((def (list #f)))
    (lambda (x)
      (not (eq? (getprop x '*toplevel* def) def)))))

(define eval-cur-env eval)

; ---------------------------------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.

;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght three, or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (witout extension and initial path).
;; KN, December 2005: You should probably call the function file-name-proper on the second constituent of 
;; the list returned by this laml-canonical-command-line.
;; Element number three must be a slash terminated directory, in the source file resides.
;; This function must be redefined in scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (getprop 'argv '*environment-variables*))

;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in scheme-system dependent compatibility file
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
;; .misc program parameters are not supported in SISC yet.
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (let ((program-parameters (optional-parameter 1 optional-parameter-list '())))
   (putprop 'argv '*environment-variables* (list 'laml source-file startup-dir program-parameters))))
