;;;; This program configures your LAML system. By calling the procedure
;;;; laml-config (without parameters) you read the configuration file
;;;; "configuration" from this directory, and overwrites the following files
;;;; (relative to the laml directory): laml.scm, laml.el, laml.init, bin/laml.bat,
;;;; bin/laml, and /emacs-support/laml-emacs-support.el. It is also possible
;;;; to have your emacs init file (.emacs) updated with LAML support.
;;;; The files bin/laml and bin/laml.bat are command files with which to start laml
;;;; from a command prompt.
;;;; 
;;;; Usage:
;;;;   1. Load this file from the current directory.
;;;;   2. Call: (laml-config).
;;;;   3. If errors are located in your configuration the program
;;;;      reports them. Correct them and goto back to step 2.
;;;;   4. Exit Scheme.
;;;;
;;;; You can call (laml-clean) to delete all generated files.
;;;; See also the WWW installation guide for further information.
;;;; 
;;;; This program assumes the existence of the non-RS4S functions file-exists?, 
;;;; directory-exists? and delete-file.

(define directory-exists? file-exists?)
(define delete-file sys-unlink)


; --------------------------------------------------------------------------------------------------------

; Here we define optional-parameter (from laml.scm) because write-text-file uses it:
(define (optional-parameter n optional-parameter-list . optional-default-value)
  (let ((optional-default-value-1 (if (null? optional-default-value) #f (car optional-default-value)))) 
   (if  (> n (length optional-parameter-list))
      optional-default-value-1
      (list-ref optional-parameter-list (- n 1)))))

; --------------------------------------------------------------------------------------------------------

(define supported-platforms '(windows unix))
(define supported-scheme-systems '(scm mzscheme guile drscheme mzscheme-200 mzscheme-300 sisc bigloo gambit-4 drscheme-300 gosh))
                                                                 ; mzscheme really means version 103 or smaller.
                                                                 ; drscheme similarly means version 103 or smaller. 
                                                                 ; mzscheme-200 means version 200 - 209
                                                                 ; mzscheme-300 means version 299, and 300+
                                                                 ; drscheme-300 means DrScheme 300+
                                                                 ; gosh         means Gauche

(define supported-operating-systems '(win98 win95 nt40 solaris-6 linux solaris-7 win2000 osx))
(define supported-kinds-of-emacs '(gnu-emacs win-emacs)) ; not important any more

(load "../lib/general.scm")
(load "../lib/file-read.scm")

; Redefinition from general - a simpler version which does not depend on the optional-parameter function:
(define (file-read file-name)
 (let* ((port (open-input-file file-name))
        (contents (read port)))
   (close-input-port port)
   contents))

; Here is optional-parameter anyway, because of write-text-file which uses it:
(define (optional-parameter n optional-parameter-list . optional-default-value)
  (let ((optional-default-value-1 (if (null? optional-default-value) #f (car optional-default-value))))  ; the old fashioned vay of handling it...
   (if  (> n (length optional-parameter-list))
      optional-default-value-1
      (list-ref optional-parameter-list (- n 1)))))


(display-message "The LAML Configuration Program - please wait...")
; (display (as-string #\newline))

; (display-message "While you wait let me explain what is going on...")
; (display-message "Based on your configuration description the LAML configuration program")
; (display-message "now writes customized versions of laml.scm, laml.el, laml.init")
; (display-message "and emacs-support/laml-emacs-support.el. If you asked for it, the program")
; (display-message "will also modify your .emacs file.")

(display (as-string #\newline))

(define config-alist '())  ; reassigned later.

; A list of file-descriptor. A file descriptor is itself a list
; of template file (unconfigured) and destination file (configured).
; The file descriptors identify source and target files of (part of) the configuration.
; The laml command files as well as the laml emacs command files are mangaged by other means 
; (see configure-laml-exec! and configure-laml-execute!).
(define setup-file-descriptors-1
  (list 
     (list "templates/laml.scm" "../laml.scm")
     (list "templates/laml.el" "../laml.el")
     (list "templates/laml.init" "../laml.init")
     (list "templates/laml-emacs-support.el" "../emacs-support/laml-emacs-support.el")
     (list "templates/dot-emacs-insertion.el" "temp/dot-emacs-insertion.el")
  ))

; Like setup-file-descriptors-1, but do not add an 'auto generated comment' to the files in this list
(define setup-file-descriptors-2
  (list 
     (list "templates/dot-emacs-insertion.el" "temp/dot-emacs-insertion.el")
  ))

; Alleviate some of the trivial problems that can arrise.
; These are confusions about symbols or strings, terminations of file paths, etc.
(define (be-helpful-and-expand config-alist)
 (let* (
        (laml-version (read-text-file (string-append "../distribution-version")))
        (laml-dir  (get 'laml-dir config-alist))
        (scheme-system  (as-string (get 'scheme-system  config-alist)))
        (laml-platform  (as-string (get 'laml-platform config-alist)))
        (operating-system  (as-string (get 'operating-system config-alist)))
        (scheme-exec  (get 'scheme-exec config-alist))
        (emacs-init-file  (get 'emacs-init-file config-alist))
        (laml-init-file (defaulted-get 'laml-init-file config-alist #f))
        (leno-emacs-support (defaulted-get 'leno-emacs-support config-alist #f))
        (photoshow-emacs-support (defaulted-get 'photoshow-emacs-support config-alist #f))
        (chords-emacs-support (defaulted-get 'chords-emacs-support config-alist #f))
        (elucidator-emacs-support (defaulted-get 'elucidator-emacs-support config-alist #f))
        (schemedoc-emacs-support (defaulted-get 'schemedoc-emacs-support config-alist #f))
        (command-scheme-exec                                  
          (if scheme-exec
            (if (eq? 'windows (as-symbol laml-platform))
		(defaulted-get 'command-scheme-exec config-alist ; derived from scheme-exec. Slash -> Backslash
		  (replace-string scheme-exec (as-string #\/) (as-string #\\)))
		(defaulted-get 'command-scheme-exec config-alist scheme-exec))
            #f))
        (laml-library                                         ; only in strange situations different from lib 
          (defaulted-get 'laml-library config-alist "lib"))  
        (computer-system                                      ; probably only important for normark 
          (defaulted-get 'computer-system config-alist "cs-unix"))
        (emacs-keybindings (defaulted-get 'emacs-keybindings config-alist "original"))
       )
    (pair-up
      (list 'laml-dir 'scheme-system 'laml-platform 'operating-system 'scheme-exec 'command-scheme-exec 'emacs-init-file
            'laml-library 'computer-system 'laml-version 'laml-init-file 'leno-comment-glyph 'elucidator-comment-glyph 
            'emacs-keybindings 'photoshow-comment-glyph 'chords-comment-glyph 'schemedoc-comment-glyph)
      (list (if (slash-terminated? laml-dir) laml-dir (string-append laml-dir "/"))
            (as-symbol (downcase-string scheme-system))
            (as-symbol (downcase-string laml-platform))
            (as-symbol (downcase-string operating-system))
            (if scheme-exec (as-string scheme-exec) #f)
            (if command-scheme-exec (as-string command-scheme-exec) #f)
            (if emacs-init-file (as-string emacs-init-file) #f)
            (as-string laml-library)
            (as-symbol computer-system)
            (as-string laml-version)
            laml-init-file
            (if leno-emacs-support "" ";")         ; char to insert in laml-emacs-support.el in front of leno support loading
            (if elucidator-emacs-support "" ";")   ; char to insert in laml-emacs-support.el in front of elucidator support loading
            (as-symbol (downcase-string emacs-keybindings))
            (if photoshow-emacs-support "" ";")
            (if chords-emacs-support "" ";")
            (if schemedoc-emacs-support "" ";")
     ))))

(define (do-nothing)
  'nothing)

(define platform-problem? #f)
(define emacs-support-problem? #f)
(define laml-command-file-problem? #f)
(define schemedoc-command-file-problem? #f)
(define absolute-laml-command-file-problem? #f)

; Perform the laml configuration from the information in setup-file-descriptors (1 and 2) and
; configuration. The parameter configuration defaults to the global variable config-alist.
; This procedure overwrites a number of files in the root of the laml directory
; and in the laml bin directory.
(define (laml-config . configuration)
  (set! wm 0)
  (let* ((config-alist-0 (if (not (null? configuration)) (car configuration) (file-read "configuration")))
         (config-alist-1 (be-helpful-and-expand config-alist-0))
         (laml-dir (get 'laml-dir config-alist-1))
         (scheme-system (as-symbol (get 'scheme-system  config-alist-1))) 
        )
   (check-configuration! config-alist-1)

   (display-message "Scheme system: " (as-string (get 'scheme-system config-alist-1))
                  "       Platform: " (as-string (get 'laml-platform config-alist-1))
                  "       Operating system: " (as-string (get 'operating-system config-alist-1)))
   (display-message "LAML directory:" (as-string (get 'laml-dir config-alist-1)))

   (map (lambda (file-descriptor)
            (configure-file-1 (car file-descriptor) config-alist-1 (cadr file-descriptor)))
         setup-file-descriptors-1)

   (map (lambda (file-descriptor)
            (configure-file-2 (car file-descriptor) config-alist-1 (cadr file-descriptor)))
         setup-file-descriptors-2)

   ; December 3, 2003: Due to changes of the E-lisp LAML processing, configuration specific
   ; LAML execution files are no longer copied from ./emacs-stuff to emacs-support/laml-execute.el 
   ; (configure-laml-execute! config-alist-1)  ; assigns emacs-support-problem?

   (configure-laml-exec! config-alist-1)     ; assigns laml-command-file-problem?
   (configure-schemedoc-exec! config-alist-1)     ; assigns schemedoc-command-file-problem?

   (configure-absolute-laml-exec! config-alist-1)   ; assigns absolute-laml-command-file-problem?
   (if (not emacs-support-problem?) (configure-dot-emacs! config-alist-1))

   (if (or (> wm 0) platform-problem? emacs-support-problem? laml-command-file-problem?)
       (display-message "The LAML configuration files are now redefined, but probably not correct yet.")
       (do-nothing))
      

   (display-message
     (string-append (if (or (> wm 0) platform-problem? emacs-support-problem? laml-command-file-problem?)
                        (string-append 
                          "You should fix the problems reported above and load laml-config.scm again."
                          (as-string #\newline)
                          "If you ignore the warnings, you can carry out the final installation step in the LAML installation guide.")
                        (string-append (as-string #\newline) "*** LAML HAS BEEN CONFIGURED SUCCESSFULLY - CONGRATULATIONS ***"  (as-string #\newline)))))

   (display-message (emacs-fate))

   (if (as-boolean (defaulted-get 'emacs-init-file config-alist-0 #f))
       (begin 
         (display (as-string #\newline))
         (display-message "If you dislike some of the Emacs key bindings you can change many")
         (display-message "of them in the file " (string-append laml-dir "emacs-support/laml-key-menu-bindings.el"))
       )
   )

;    (if (not (get 'leno-emacs-support config-alist-0))
;        (begin 
;          (display (as-string #\newline))
;          (display-message "If you eventually will try out LENO you can")
;          (display-message "re-configure LAML with LENO Emacs support."))
;    )
; 
; 
;    (if (not (get 'elucidator-emacs-support config-alist-0))
;        (begin 
;          (display (as-string #\newline))
;          (display-message "If you eventually wish to use the Scheme elucidator, you can")
;          (display-message "re-configure LAML with elucidator Emacs support."))
;   )

   (if (not (or (> wm 0) platform-problem? emacs-support-problem? laml-command-file-problem?))
       (begin
         (if (eq? (get 'laml-platform config-alist-0) 'unix)
             (begin
                 (display (as-string #\newline))
                 (display-message  "As the final step in the LAML installation/configuration process")
                 (display-message  "now change protection of the executable files in the LAML bin directory.")))

         (if (not (memq scheme-system '(drscheme drscheme-300)))
             (begin
	       (display (as-string #\newline))
	       (display-message  "If you plan to use the operating system laml commands in the bin directory")
	       (display-message  "you should add the LAML bin directory to your path.")
	       (display-message  "You should also care about the protection of the files in bin.")))

         (display (as-string #\newline))
         (display-message  "You can re-configure LAML at a later point in time if you wish.")
         (display-message  "This may be relevant if you choose to use another Scheme system.")
       )
   )
))


;; Write the appropriate laml-execute to the emacs-support directory.
;; The appropriate file is determined by laml-platform, operating-system, and scheme-system.
;; mzscheme-200 and mzscheme-300 are translated to mzscheme when selecting laml-execute emacs support.
;; NOT used anymore.
(define (configure-laml-execute! configuration)
  (let* ((scheme-system (get 'scheme-system  configuration))
         (laml-platform (get 'laml-platform configuration))
         (operating-system (get 'operating-system configuration))
         (preferred-laml-execute 
            (if (or (eq? scheme-system 'mzscheme-200) (eq? scheme-system 'mzscheme-300))
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_"
                               (as-string 'mzscheme) "_" "laml-execute.el")
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_"
                               (as-string scheme-system) "_" "laml-execute.el")))
         (source-laml-execute 
            (if (file-exists? (string-append "emacs-stuff/" preferred-laml-execute))
                (string-append "emacs-stuff/" preferred-laml-execute)
                (string-append "emacs-stuff/" "default-laml-execute.el")))
         (target-laml-execute "../emacs-support/laml-execute.el")
        )
    (if (file-exists? target-laml-execute) (delete-file target-laml-execute))
    (write-text-file (read-text-file source-laml-execute) "../emacs-support/laml-execute.el")
    (if (file-exists? (string-append "emacs-stuff/" preferred-laml-execute))
        (set! emacs-support-problem? #f)
        (begin
          (display-message "LAML cannot be configured to support Emacs LAML execution.")
          (display-message "Reason: There is no Emacs Lisp implementation of the LAML activation stuff for the configuration.")
          (set! emacs-support-problem? #t)
        )
    )
  ))

;; Write a configured version of the appropriate laml or laml.bat to the bin directory.
;; The appropriate file is determined by laml-platform, operating-system, and scheme-system.
;; mzscheme-200 and mzscheme-300 are translated to mzscheme when selecting laml-execute emacs support.
(define (configure-laml-exec! configuration)
  (let* ((scheme-system (get 'scheme-system  configuration))
         (laml-platform (get 'laml-platform configuration))
         (operating-system (get 'operating-system configuration))
         (preferred-laml-exec 
            (if (or (eq? scheme-system 'mzscheme-200) (eq? scheme-system 'mzscheme-300))
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string 'mzscheme) "_" "laml")
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string scheme-system) "_" "laml")))
         (source-laml-exec 
            (if (file-exists? (string-append "exec-stuff/" preferred-laml-exec))
                (string-append "exec-stuff/" preferred-laml-exec)
                (if (not platform-problem?)
                    (string-append "exec-stuff/" (string-append (as-string laml-platform) "-" "default-laml"))
                    (string-append "exec-stuff/" (string-append "unix" "-" "default-laml")))))
         (target (if (eq? laml-platform 'windows) "laml.bat" "laml"))
         (target-laml-exec (string-append "../bin/" target))
        )
    (if (file-exists? target-laml-exec) (delete-file target-laml-exec))
    (write-text-file (configure-string (read-text-file source-laml-exec) configuration) target-laml-exec)
    (if (or (file-exists? (string-append "exec-stuff/" preferred-laml-exec)) (memq scheme-system '(drscheme drscheme-300)))   ; Do not complain of missing command file for DrScheme.
        (set! laml-command-file-problem? #f)
        (begin 
           (display-message "LAML cannot be configured with a laml command prompt/shell file.")
           (display-message "Reason: There is no implementation of a (shell/bat) command file for the configuration.")
           (set! laml-command-file-problem? #t)
        )
    )
  ))

;; Write a configured version of the appropriate schemedoc or schemedoc.bat to the bin directory.
;; The appropriate file is determined by laml-platform, operating-system, and scheme-system.
;; mzscheme-200 and mzscheme-300 are translated to mzscheme.
(define (configure-schemedoc-exec! configuration)
  (let* ((scheme-system (get 'scheme-system  configuration))
         (laml-platform (get 'laml-platform configuration))
         (operating-system (get 'operating-system configuration))
         (preferred-schemedoc-exec 
            (if (or (eq? scheme-system 'mzscheme-200) (eq? scheme-system 'mzscheme-300))
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string 'mzscheme) "_" "schemedoc")
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string scheme-system) "_" "schemedoc")))
         (source-schemedoc-exec 
            (if (file-exists? (string-append "exec-stuff/" preferred-schemedoc-exec))
                (string-append "exec-stuff/" preferred-schemedoc-exec)
                (if (not platform-problem?)
                    (string-append "exec-stuff/" (string-append (as-string laml-platform) "-" "default-schemedoc"))
                    (string-append "exec-stuff/" (string-append "unix" "-" "default-schemedoc")))))
         (target (if (eq? laml-platform 'windows) "schemedoc.bat" "schemedoc"))
         (target-laml-exec (string-append "../bin/" target))
        )
    (if (file-exists? target-laml-exec) (delete-file target-laml-exec))
    (write-text-file (configure-string (read-text-file source-schemedoc-exec) configuration) target-laml-exec)
    (if (or (file-exists? (string-append "exec-stuff/" preferred-schemedoc-exec)) (memq scheme-system '(drscheme drscheme-300)))
        (set! schemedoc-command-file-problem? #f)
        (begin 
;           Do not (yet) confuse the user with SchemeDoc command problems.
;           (display-message "LAML cannot be configured with a schemedoc command file.")
;           (display-message "Reason: There is no implementation of a (shell/bat) command file for the configuration.")
           (set! schemedoc-command-file-problem? #t)
        )
    )
  ))

;; Write a configured version of the appropriate absolute-laml or absolute-laml.bat to the bin directory.
;; The appropriate file is determined by laml-platform and scheme-system.
;; mzscheme-200 and mzscheme-300 are translated to mzscheme when selecting laml-execute emacs support.
(define (configure-absolute-laml-exec! configuration)
  (let* ((scheme-system (get 'scheme-system  configuration))
         (laml-platform (get 'laml-platform configuration))
         (operating-system (get 'operating-system configuration))
         (preferred-laml-exec 
            (if (or (eq? scheme-system 'mzscheme-200) (eq? scheme-system 'mzscheme-300))
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string 'mzscheme) "_" "absolute-laml")
                (string-append (as-string laml-platform) "_" (as-string operating-system) "_" (as-string scheme-system) "_" "absolute-laml")))
         (source-laml-exec 
            (if (file-exists? (string-append "absolute-exec-stuff/" preferred-laml-exec))
                (string-append "absolute-exec-stuff/" preferred-laml-exec)
                (if (not platform-problem?)
                    (string-append "absolute-exec-stuff/" (string-append (as-string laml-platform) "-" "default-absolute-laml"))
                    (string-append "absolute-exec-stuff/" (string-append "unix" "-" "default-absolute-laml")))))
         (target (if (eq? laml-platform 'windows) "absolute-laml.bat" "absolute-laml"))
         (target-laml-exec (string-append "../bin/" target))
        )
    (if (file-exists? target-laml-exec) (delete-file target-laml-exec))
    (write-text-file (configure-string (read-text-file source-laml-exec) configuration) target-laml-exec)
    (if (or (file-exists? (string-append "absolute-exec-stuff/" preferred-laml-exec)) (memq scheme-system '(drscheme drscheme-300)))
        (begin
          (set! absolute-laml-command-file-problem? #f)
;          (display-message "LAML has - as a supplementary means - been configured with an absolute-laml command file.")
        )
        (begin 
           (set! absolute-laml-command-file-problem? #t)
        )
    )
  ))

(define dot-emacs-action #f)

; Modify the users .emacs file such that it loads the laml-emacs-support.el file
(define (configure-dot-emacs! configuration)
  (let ((emacs-init-file (get 'emacs-init-file configuration))
        (laml-dir (get 'laml-dir configuration))
        (laml-dot-emacs-string (read-text-file "temp/dot-emacs-insertion.el")))
    (if emacs-init-file
        (if (file-exists? emacs-init-file)
            (let ((existing-dot-emacs-string (read-text-file emacs-init-file))
                 )
              (if (not (already-configured? existing-dot-emacs-string laml-dir))
                  (begin
                   (if (trace-of-earlier-laml-installation? existing-dot-emacs-string)
                       (begin
                         (display-message "You must delete an earlier LAML installation from your  .emacs  file")
                         (display-message "After fixing this Emacs problem, exit and restart Emacs.")))
                   (write-text-file
                    (string-append 
                       existing-dot-emacs-string
                       laml-dot-emacs-string)
                    emacs-init-file)
                   (set! dot-emacs-action 'updating))
                  (set! dot-emacs-action 'already-ok)))
            (begin
              (write-text-file
                 laml-dot-emacs-string
                 emacs-init-file)
              (set! dot-emacs-action 'new)))
         (set! dot-emacs-action 'nothing))))

; Is there a loading of dot-emacs-contribution.el in existing-dot-emacs-string?
(define (already-configured? existing-dot-emacs-string laml-dir)
  (let ((search-for-string (string-append "(load " (as-string #\") (as-string laml-dir) 
                                          "emacs-support/laml-emacs-support.el"
                                          (as-string #\") ")")))
    (if (substring-index existing-dot-emacs-string 0 search-for-string)
        #t #f)))

(define (trace-of-earlier-laml-installation? existing-dot-emacs-string)
  (let ((search-for-string "laml-emacs-support.el"))
    (if (substring-index existing-dot-emacs-string 0 search-for-string)
        #t #f)))


; Return a string with a message: what happened to .emacs
(define (emacs-fate)
 (cond ((eq? dot-emacs-action 'nothing) "Emacs has not been configured with LAML support. Your  .emacs  file has not been modified.")
       ((eq? dot-emacs-action 'new) "A new  .emacs  file has been created with LAML support.")
       ((eq? dot-emacs-action 'updating) "The already existing  .emacs  file has been updated with LAML support.")
       ((eq? dot-emacs-action 'already-ok) "The existing  .emacs  file already had correct LAML support.")
       (else "")))
  
               

(define (slash-terminated? str)
  (eqv? (string-ref str (- (string-length str) 1)) #\/))

(define wm 0)

(define (warn-notedly . msg-elements)
  (display-warning (apply string-append (map as-string msg-elements)))
  (set! wm (+ 1 wm)))

(define (list-to-string-1 lst)
  (list-to-string (map as-string lst) ", "))

(define (check-configuration! configuration)
  (let (
        (laml-dir  (get 'laml-dir configuration))
        (scheme-system  (get 'scheme-system  configuration))
        (laml-platform  (get 'laml-platform configuration))
        (operating-system  (get 'operating-system configuration))
        (scheme-exec  (get 'scheme-exec configuration))
        (command-scheme-exec  (get 'command-scheme-exec configuration))
        (emacs-init-file (get 'emacs-init-file configuration))
        (laml-library  (get 'laml-library configuration))
        (computer-system  (get 'computer-system configuration))
        (laml-init-file (get 'laml-init-file configuration))
        (emacs-keybindings (get 'emacs-keybindings configuration))
       )
   (if (not (slash-terminated? laml-dir))
       (begin (display-warning (string-append "last char in laml-dir must be a '/': " laml-dir))
              (set! wm (+ wm 1))))

   (if (and scheme-exec (slash-terminated? scheme-exec))
       (warn-notedly "last char in scheme-exec must not be a '/': " scheme-exec))

   (if (not (memq laml-platform supported-platforms))
       (begin
         (warn-notedly "laml-platform must be one of "
		      (list-to-string-1 supported-platforms) ". Current value: " laml-platform)
         (set! platform-problem? #t))
       (set! platform-problem? #f))

   (if (not (directory-exists? laml-dir))
       (warn-notedly "The laml directory is non-existing: " laml-dir))

   (if (and scheme-exec (not (or (file-exists? (string-append scheme-exec)) (file-exists? (string-append scheme-exec ".exe")))))
       (warn-notedly "The Scheme executable does not exist as a file name: " (as-string scheme-exec) (as-string #\newline) 
                     "If you have given a simple command name, which is in you path, you can perhaps ignore this warning.")) 
         

   (if (not (memq scheme-system supported-scheme-systems))
       (warn-notedly "scheme-system must be one of "
 		      (list-to-string-1 supported-scheme-systems) ". Current value: " scheme-system))

   (if (not (memq operating-system supported-operating-systems))
       (warn-notedly "operating system must be one of "
		      (list-to-string-1 supported-operating-systems) ". Current value: " operating-system))

   (if (not (directory-exists? (string-append laml-dir laml-library)))
       (warn-notedly "The laml library directory is non-existing: "
                            (string-append laml-dir laml-library "/")))

   (if (not (or (string? emacs-init-file) 
                (and (boolean? emacs-init-file) (eq? emacs-init-file #f))))
       (warn-notedly "The emacs-init-file must be a full path to your .emacs file (a string) or the boolean value #f"))

   (if (not (or (and (boolean? laml-init-file) (not laml-init-file))
                (string? laml-init-file))  ; not necessarily existing!
       )
       (warn-notedly "The laml-init-file must be a full path to your .laml file (a string) or the boolean value #f"))

   (if (not (memq emacs-keybindings (list 'original 'hygienic 'none)))
       (warn-notedly 
         (string-append "The emacs-keybinding property  " 
                        (as-string emacs-keybindings) "  must be either \"original\", \"hygienic\", or \"none\"") ))

  ))

(define (auto-generated-lisp-comment code)
  (string-append
    "; This file is made during the LAML configuration process - DO NOT EDIT!"
    (as-string #\newline)  (as-string #\newline)
    code))
                

; Configure file f (relative path from this directory) using config-alist.
; Write result to destination-path, thereby overwriting it if it exists already.
(define (configure-file-1 f config-alist destination-path)
  (let* ((file-text (read-text-file f))
         (configured-file-text (auto-generated-lisp-comment (configure-string file-text config-alist)))
        )
    (if (file-exists? destination-path)
        (delete-file destination-path))
    (write-text-file configured-file-text destination-path)))

; Like configure-file-2, but do not insert auto generation comment
(define (configure-file-2 f config-alist destination-path)
  (let* ((file-text (read-text-file f))
         (configured-file-text (configure-string file-text config-alist))
        )
    (if (file-exists? destination-path)
        (delete-file destination-path))
    (write-text-file configured-file-text destination-path)))

(define (configure-string str config-alist)
  (if (null? config-alist)
      str
      (configure-string
        (configure-one-option str (car config-alist))
        (cdr config-alist))))

(define (configure-one-option str config-pair)
  (let ((search-string (string-append "@" (as-string (car config-pair)) "@"))
        (replacement (as-string (cdr config-pair))))
    (replace-string str search-string replacement)))

;; Clean the effect of the configuration process    
;; Deletes files in your laml configuration.
(define (laml-clean)
  (for-each delete-file (map cadr (append setup-file-descriptors-1 setup-file-descriptors-2))))













