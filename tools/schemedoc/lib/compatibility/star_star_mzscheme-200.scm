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


;;;; .title Reference Manual of the MzScheme LAML compatibility libray.
;;;; MzScheme specific stuff to be loaded for compatibility. 
;;;; This file implements each of the necessary non-R5RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in MzScheme. These are 
;;;; file-exists?, delete-file, copy-file, and directory-exists?
;;;; Therefore, these functions need not to be provided in this compatibility library. 

(require (lib "compat.ss"))
(require (lib "url.ss" "net"))

;;; Definition of non-R5RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R5RS). 

;; Return the current time in seconds
(define (current-time)
  (current-seconds))

;; Sort list using the comparison predicate
(define (sort-list list com)
  (if (null? list) list (sort com list)))


;; Make a new directory, new-dir, in the directory path (first parameter).
;; The parameter in-directory-path ends in a slash.
(define (make-directory-in-directory in-directory-path new-dir)
  (make-directory (string-append in-directory-path new-dir)))

;; Mail sending support: Send an email to a receiver.
;; Not implemented on mzscheme on the PC platform.
(define (mail receiver title contents)
  (error "The mail function is not implemented in MzScheme on the PC platform"))

; -----------------------------------------------------------------------------

;; Is symbol bound in the current interaction environment.
(define (bound? symbol)
  (if (memq symbol (namespace-mapped-symbols)) #t #f))

;; Eval the expression e in the current interaction environment.
;; .form (eval-cur-env e)
(define eval-cur-env eval)

; Version 29: Does not work fully correct. Reports success for existing server but non-existing file.
;(define (url-target-exists? url-string)
;  (with-handlers ((exn?
;                     (lambda (exn) #f)))
;     (let ((ip (get-pure-port (string->url url-string))))
;       (close-input-port ip)
;       #t)))

;; Return whether the target of the url, as represented by url-string, exits.
;; Notice that this version of url-target-exists? is rather expensive, because it actually transfers the document over the network.
;; It also requires the LAML library url-read.scm, which is not part of the standard LAML distribution.
(define (url-target-exists? url-string)
  (let* ((result (read-http-alist url-string))
         (status (as-number (defaulted-get 'status result "0"))))
    (cond ((and (>= status 200) (<= status 399)) #t)
          (else #f))))


;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.


;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght four or #f if no command line activation exists.
;; The first element must be the symbol laml (a tag).
;; Element number two must be the laml source file name (without extension and initial path).
;; Element number three must be a slash terminated, full directory path (with forward slashes), in which the source file resides.
;; Element number four must be a list of program parameters.
;; This function must be redefined in Scheme-system/OS/platform dependent compatibility file.
(define (laml-canonical-command-line)
  (if (and (vector? argv) (>= (vector-length argv) 2))
        (list 'laml 
               (file-name-proper (vector-ref argv 0))
               (transliterate (vector-ref argv 1) #\\ "/") ; ensure forward slashing
               (if (>= (vector-length argv) 3) (vector-ref argv 2) '())
        )
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

(read-case-sensitive #t)

