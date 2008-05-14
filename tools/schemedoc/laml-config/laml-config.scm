;;;; This program configures your LAML system. By loading this file you implicitly call
;;;; laml-config (without parameters). It reads the configuration file
;;;; "configuration" from this directory, and overwrites the following files
;;;; (relative to the laml directory): laml.scm, laml.el, laml.init, bin/laml.bat,
;;;; bin/laml, and /emacs-support/dot-emacs-contribution.el. It is also possible
;;;; to have your emacs init file updated with LAML support.
;;;; The files bin/laml and bin/laml.bat are command files with which to start laml
;;;; from a command prompt.
;;;; 
;;;; Usage:
;;;;   1. Load this file from the current directory.
;;;;   2. If errors are located in your configuration the program
;;;;      reports them. Correct them and redo step 1.
;;;;   3. Exit Scheme.
;;;;
;;;; You can call (laml-clean) to delete all generated files.
;;;; See also the WWW installation guide for further information.
;;;; 
;;;; This program assumes the existence of the non-RS4S functions file-exists?, 
;;;; directory-exists? and delete-file.

(load "laml-config-internal.scm")
(laml-config) ; using the file named "configuration"