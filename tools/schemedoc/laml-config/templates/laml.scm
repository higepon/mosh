;;;; .title Reference Manual of the LAML library
;;;; The file <kbd>laml.scm</kbd> is the very first laml file to load.
;;;; It contains a number of variable and functions which must be defined whenever LAML is used.
;;;; Some of the variables - the configuration variables - are defined via the LAML configuration process.
;;;; It also contains some very fundamental LAML stuff, including a number of top level commands that activate LAML tools.
;;;; Physically, laml.scm is composed of a tiny, configuration dependent file followed by the file laml-fundamental.scm.
;;;; These two files are documented together.<p>
;;;;
;;;; <kbd>laml.scm</kbd> loads 
;;;; the scheme/OS/platform specific compatibility file
;;;; and the <a href="../lib/man/general.html">general library</a> from the <kbd>lib</kbd> directory.
;;;;
;;;; It is assumed that the value of the variable <kbd>laml-dir</kbd> is the full path of the LAML directory;
;;;; <kbd>laml-dir</kbd> must be defined when <kbd>laml.scm</kbd> is loaded, and the path must end in a "/".
;;;; The laml command prompt command and the LAML Emacs activation commands will take care of the definition of <kbd>laml-dir</kbd> for you.


; Physically, this file constitute the configuration dependent part.
; The configuration independent part are located in laml-fundamental.scm

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2006  Kurt Normark, normark@cs.aau.dk.
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


;;; The configuration section.
;;; The configuration section is meant to be addapted in each new LAML installation.
;;; This section contains a few fundamental variables. The variables are defined via
;;; the configuration file in the laml-config directory. 
;;; .section-id config-section

; ---------------------------------------------------------------------------------------------------
; CONFIGURATION SECTION:

;; The Scheme system on which LAML depends (a symbol).
;; Possible values are: mzscheme, scm, guile, drscheme.
;; The value is frozen by the LAML configuration program.
(define scheme-system '@scheme-system@)

;; The platform on which LAML is in use (a symbol).
;; Possible values are: windows, unix, mac. mac is not yet in use.
;; The value is frozen by the LAML configuration program.
(define laml-platform '@laml-platform@)

;; The operating system on which LAML is in use (a symbol).
;; Possible values on the windows platform: win98, win95, nt40, win2000.
;; Possible values on the unix platform: solaris-6, solaris-7, or linux.
;; The value is frozen by the LAML configuration program.
(define operating-system '@operating-system@)

;; The scheme library relative to laml-dir. A string.
;; A single directory name (without ending slash).
;; The value is frozen by the LAML configuration program.
;; You can change this if you use an alternative or experimental LAML library.
(define laml-library "@laml-library@")

;; A variable that refers to the version of LAML, bound at LAML installation time.
;; .returns A string that contains the version number and a short description.
(define laml-version "@laml-version@")


; Determines how the laml processing is initiated. A symbol.
; rich: Information is transferred from the context such that the Scheme system knows the 
; file name and the start-up directory.
; poor: No information is transferred from the context.
; We now always use the value rich - in reality this variable does not play a role any longer.
(define laml-activation 'rich)


; The default name of an LAML output file.
; In case laml-activation is 'rich we use the name of the laml file to determine the name of the html output file.
; In poor laml activation situations we use the value of the variable laml-default-output-file.
; A file name without initial path and without extension.
; This variable does not play a role any longer.
(define laml-default-output-file "default")

; The default directory of LAML output.
; In case laml-activation is 'rich we use the name of the startup directory 
; (as returned by (startup-directory scheme-system) to determine this directory.
; The variable laml-default-output-directory is only used in case of poor laml-activation.
; This variable does not play a role any longer.
(define laml-default-output-directory "")

; The machine on which I use LAML-based software.
; This variable is not used any place in the LAML software, so *you* can forget about.
; I use it in the setup files of the LENO and course-plan systems. The variable allows
; me to find out on which machine I am running. Files, on which I rely, may be placed different
; places on different machines.
; Possible values: cs-unix, home-pc, thinkpad
(define computer-system '@computer-system@)

;; The function begin-laml is supposed to be called at the time all LAML software loading is done.
;; The function is also available via the alias end-laml-loading.
;; The function loads the LAML init file, typically called .laml.
;; By loading the LAML init file when all software is loaded, it is always possible to redefine crucial parts in the LAML init file. 
;; In HTML, SVG and similar contexts it is the responsibility of the document author to call begin-laml (as well as end-laml).
;; In contexts where we process a more elaborate XML-in-LAML document, the software that processes the document calls begin-laml.
;; .internal-references "alias function" "end-laml-loading"
;; .internal-references "related function" "end-laml"
(define (begin-laml)
  ; Load the user's laml init file, if specified in the configuration file, and if the file exists.
  (if (and (not (equal? "@laml-init-file@" "false")) (file-exists? "@laml-init-file@"))
    (load "@laml-init-file@")))

;; An alias of begin-laml.
;; Rationale: In some contexts it is natural to state the point where all LAML loading is done.
(define end-laml-loading begin-laml)

; END CONFIGURATIONS.

; ---------------------------------------------------------------------------------------------------------------------------------------------------------

; ADDITIONAL LOADING

; Load the relevant LAML compatibility file.
(let ((laml-lib-comp-file (lambda (nm) (string-append laml-dir "lib/compatibility/" nm)))
      (comp-file (lambda (nm) (string-append laml-dir "lib/compatibility/" nm)))
      (schemesys-platform-os (string-append (symbol->string laml-platform) "_" (symbol->string operating-system) "_" (symbol->string scheme-system) ".scm"))
      (schemesys-platform-star (string-append (symbol->string laml-platform) "_" "star" "_" (symbol->string scheme-system) ".scm"))
      (schemesys-star-star (string-append "star" "_" "star" "_" (symbol->string scheme-system) ".scm"))
     )
  (cond ((file-exists? (laml-lib-comp-file schemesys-platform-os))
             (load (comp-file schemesys-platform-os)))
        ((file-exists? (laml-lib-comp-file schemesys-platform-star))
             (load (comp-file schemesys-platform-star)))
        ((file-exists? (laml-lib-comp-file schemesys-star-star))
             (load (comp-file schemesys-star-star)))
        (else (error (string-append "Compatibility loading: Cannot find compatibility file in lib/compatibility.")))))

; Load the general LAML library
(load (string-append laml-dir "lib/general.scm"))

; Load the configuration independent part of the fundamental LAML library. 
(load (string-append laml-dir "laml-fundamental.scm"))

; (if (and (not (equal? "@laml-init-file@" "false")) (file-exists? "@laml-init-file@"))
;    (load "@laml-init-file@"))