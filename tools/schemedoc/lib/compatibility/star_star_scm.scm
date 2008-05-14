; A copy of unix_solaris-7_scm.scm

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


;;;; SCM specific stuff to be loaded for compatibility. 
;;;; This file implements each of the necessary non-R4RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in SCM.

;;; Definition of non-R4RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R4RS).

; ---------------------------------------------------------------------------------------------------
; Current-time exists in SCM

; ---------------------------------------------------------------------------------------------------

; By loading scm.scm we issue a (require 'sort), hereby enableling access to the Scm sorting stuff.
(require 'sort)

;; Sort list by means of the comparison function com
(define (sort-list list com)
  (sort list com))

; ---------------------------------------------------------------------------------------------------
; file-exists?, delete-file exist in SCM

; ---------------------------------------------------------------------------------------------------

; Returns whether a directory exist. 
; Works both on names without and with trailing slash.
; An alias of file-exists?
(define directory-exists? file-exists?)

; ---------------------------------------------------------------------------------------------------

; Use copy-text-file for copy-file. This is not a good long term solution
(define (copy-file source destination)
   (copy-text-file source destination #t))

; ---------------------------------------------------------------------------------------------------

; Make a new directory, new-dir, in the directory path (first parameter).
; The parameter in-directory-path ends in a slash.
; This implementation works only on UNIX systems.
(define (make-directory-in-directory in-directory-path new-dir)
  (system
    (string-append
        "mkdir" " "
                (string-append in-directory-path new-dir))))

; This definition is for backward compatility for cgi applications using SCM.
; I need to go through theese and change the name of the function
(define make-directory make-directory-in-directory)


; Mail sending support: Send an email to a receiver.
; This implementation works only on UNIX Systems - not (my) Linux.
(define (mail receiver title contents)
  (write-text-file contents "temp/temp-mailfile")
  (system
    (string-append
        "/usr/ucb/mail -s " "'" title "' " receiver "< temp/temp-mailfile")))


(require 'directory)

(define (directory-list path)
 (let ((result '()))
  (directory-for-each 
   (lambda (x)
     (set! result (cons x result)))
   path
   (lambda (x)
     (and (not (equal? x ".")) (not (equal? x ".."))))
   
  )
  result))


(define (bound? x)
  (error "It has not been possible to implement the procedure bound? in SCM"))

(define eval-cur-env eval) 

; ---------------------------------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.

;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght three, or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (witout extension and initial path).
;; Element number three must be a slash terminated directory, in the source file resides.
;; This function must be redefined in scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (if (and (list? *argv*) (>= (length *argv*) 2))
      (list 'laml (file-name-proper (cadr (reverse *argv*))) (car (reverse *argv*)) '())
      #f))


;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in scheme-system dependent compatibility file
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
;; .misc program parameters are not supported in SCM yet.
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (set! *argv* (list source-file startup-dir)))









