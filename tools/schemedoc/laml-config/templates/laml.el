; Basic LAML stuff which is relevant for Emacs.
; This is also the LAML Emacs configuration file.

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark, normark@cs.auc.dk.
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

; ---------------------------------------------------------------------------------------------------
; CONFIGURATION SECTION:
; If you make a new LAML installation you are supposed to change the variables in this section.


; The directory in which the LAML Scheme package is installed.
; If LAML processing starts from Emacs (via the functions in emacs-support/laml-execute.el)
; this variable indirectly determines the value laml-dir in Scheme.
(defvar laml-dir "@laml-dir@")


; Absolute locations of you Scheme executable (absolute path to exe file).
(defvar scheme-exec "@scheme-exec@")

; The Scheme system, such as mzscheme, mzscheme-200, scm, guile, or sisc.
(defvar scheme-system '@scheme-system@)

; The platform on which you use LAML. One of the symbols 'unix or 'windows:
(defvar laml-platform '@laml-platform@)  

; Which variant of emacs do you use. 
; As of version 13 and newer, always gnu-emacs. Win-emacs is not supported any more.
(defvar kind-of-emacs 'gnu-emacs) 

; Which operating system do you use: one of the symbol 'win98, 'win95, or 'nt40 on the windows platform.
; or 'solaris on Unix.
(defvar operating-system '@operating-system@)

; Control of Emacs keybinding mode. Either the symbol original, hygienic, or none.
(defvar laml-keybinding-mode '@emacs-keybindings@)

; END CONFIGURATIONS.
; ---------------------------------------------------------------------------------------------------

; A definition similar to laml-platform. Backward compatibility with my own old stuff.

(defvar this-machine (cond ((eq laml-platform 'unix) 'sun)
                           ((eq laml-platform 'windows) 'windows)
                           (t (error "laml.el: cannot define the variable this-machine. Unknown laml platform"))))


(defun laml-load (file)
  (load (concat laml-dir file)))

(laml-load "emacs-support/laml-customize")
(laml-load "emacs-support/laml-general")
(laml-load "emacs-support/laml-mode")
(laml-load "emacs-support/laml-execute")

; Extend auto-mode-alist with mappings relevant for LAML.
(setq auto-mode-alist 
      (cons '("\\.sdoc" . schemedoc-mode)
	    (cons '("\\.chord" . chords-mode)
		  (cons '("\\.leno" . leno-mode)
			(cons '("\\.laml" . laml-mode) auto-mode-alist)))))

; ===============================================================================================================
; Platform independent helping functions in laml-execute.el

; The number of lines a source window is to be enlarged, and the Scheme output window decreased.
(defvar source-window-enlargement 10)

(defun last-buffer-line (buf)
  "Return the last line of the buffer as a string"
 (let ((p1 nil))
  (save-excursion
    (set-buffer buf)
    (goto-char (point-max))
    (previous-line 1) 
    (setq p1 (point))
    (end-of-line 1)
    (buffer-substring p1 (point)))))

(defun string-it-single (x)
  (concat "'" x "'"))

(defun string-it-double (x)
  (concat "\"" x "\""))

(defun buffer-versioned (buf-name)
 "Return whether buf-name ends with an angel bracket >"
 (let ((lgt (length buf-name)))
  (equal (substring buf-name (- lgt 1)) ">")))

(defun buffer-without-version (buf-name)
  "Assume that buf-name is a versioned name, such as xxx<2> or xxx<112>. Return the proper name"
 (let* ((lgt (length buf-name))
        (p1 nil)
        (idx 0)
       )
   (while (and (< idx lgt) (not (eq (aref buf-name idx) ?<))) (setq idx (+ idx 1)))
   (if (eq (aref buf-name idx) ?<)
       (substring buf-name 0 idx)
       (error (concat "buffer-without-version: Cannot locate '<'")))))

; --------------------------------------------------------------------------
; LAML process administration
; The idea is to kill already running laml processes in case we start a new.

(defun kill-laml-processes ()
  "Kill all running LAML processes"
  (interactive)
  (mapcar (function kill-if-laml-process) (process-list)))

(defun kill-if-laml-process (process)
  (let ((name (process-name process)))
    (if (laml-process-name-p name) (kill-process process))))

(defun laml-process-name-p (name)
  (equal (substring name 0 4) "LAML"))

; ---------------------------------------------------------------------------------------------------

(defun run-scheme-interactively ()
  "Start an interactive Scheme process. There is nothing LAML specific in the started Scheme process.
Runs the hook run-scheme-interactively-hook"
  (interactive)
  (setq inferior-lisp-mode-hook nil)
  (let ((inferior-lisp-program scheme-exec))
    (run-lisp inferior-lisp-program))
  (run-hooks 'run-scheme-interactively-hook)
)

; ---------------------------------------------------------------------------------------------------
; Stuff to run-laml-interactively

; The variable interactive-laml-mirror-library is defined in  emacs-support/laml-customize.el

(defun set-interactive-laml-mirror-library (mirror)
 (interactive (list
      (completing-read
       (concat "Existing mirror: " interactive-laml-mirror-library ". "  "New mirror (space for completions): ")
       (mapcar (function (lambda (x) (cons x nil))) (list "html-4.01" "html-4.00" "xhtml-1.0-strict" "xhtml-1.0-transitional"
                     "xhtml-1.0-frameset" "svg-1.1" "xhtml-1.1" "NONE"))
             nil t nil)))
 (setq interactive-laml-mirror-library mirror)
 (message (concat "DONE. Now do  M-x run-laml-interactively  to interact with " mirror)))


(defun selected-mirror-loading-list ()
 (cond ((equal interactive-laml-mirror-library "html-4.01")
         (append 
           (list
            "lib/html4.01-transitional-validating/basis.scm"
            "lib/html4.01-transitional-validating/surface.scm"
           )
           (if interactive-laml-load-convenience-library
               (list "lib/html4.01-transitional-validating/convenience.scm")
               nil))
       )

       ((equal interactive-laml-mirror-library "html-4.00")
         (append 
           (list
            "lib/html4.0-loose/basis.scm"
            "lib/html4.0-loose/surface.scm"
           )
           (if interactive-laml-load-convenience-library
               (list "lib/html4.0-loose/convenience.scm")
               nil))
       )

       ((equal interactive-laml-mirror-library "xhtml-1.0-strict")
         (append 
          (list
           "lib/xml-in-laml/xml-in-laml.scm"
           "lib/xml-in-laml/mirrors/xhtml10-strict-mirror.scm"
          )
          (if interactive-laml-load-convenience-library
               (list "lib/xhtml1.0-convenience.scm")
               nil)
         )
       )


       ((equal interactive-laml-mirror-library "xhtml-1.0-transitional")
         (append 
          (list
           "lib/xml-in-laml/xml-in-laml.scm"
           "lib/xml-in-laml/mirrors/xhtml10-transitional-mirror.scm"
          )
          (if interactive-laml-load-convenience-library
               (list "lib/xhtml1.0-convenience.scm")
               nil)
         )
       )


       ((equal interactive-laml-mirror-library "xhtml-1.0-frameset")
         (append 
          (list
           "lib/xml-in-laml/xml-in-laml.scm"
           "lib/xml-in-laml/mirrors/xhtml10-frameset-mirror.scm"
          )
          (if interactive-laml-load-convenience-library
               (list "lib/xhtml1.0-convenience.scm")
               nil)
         )
       )

       ((equal interactive-laml-mirror-library "xhtml-1.1")
         (append 
          (list
           "lib/xml-in-laml/xml-in-laml.scm"
           "lib/xml-in-laml/mirrors/xhtml11-mirror.scm"
          )
          (if interactive-laml-load-convenience-library
               (list "lib/xhtml1.0-convenience.scm")
               nil)
         )
       )

       ((equal interactive-laml-mirror-library "svg-1.1")
          (list
            "lib/xml-in-laml/xml-in-laml.scm"
            "lib/xml-in-laml/mirrors/svg11-mirror.scm"
          )
       )

      ((equal interactive-laml-mirror-library "NONE")
          (list))
          
      (t (error (concat "selected-mirror-loading-list: Cannot load the HTML/XML mirror: "  interactive-laml-mirror-library)))))




(defun laml-effectuate-loading (laml-file)
 (insert (concat "(load (string-append laml-dir \"" laml-file "\"))"))
 (comint-send-input))
      

; A hook function which ensures that 
; the appropriate LAML stuff is loaded after Scheme startup.
; Used for run-laml-interactively.
(defun lamlify-scheme-process ()
 (insert (concat "(define laml-dir \"" laml-dir "\")")) (comint-send-input)
 (insert "(load (string-append laml-dir \"laml.scm\"))") (comint-send-input)

 (mapcar (function laml-effectuate-loading) (selected-mirror-loading-list))

 (insert "(load (string-append laml-dir \"lib/file-read.scm\"))") (comint-send-input)
 (insert "(load (string-append laml-dir \"lib/time.scm\"))") (comint-send-input)
 (insert "(load (string-append laml-dir \"lib/color.scm\"))") (comint-send-input)
 (insert (concat "(fake-startup-parameters \"temp-file\" " "\"" the-laml-startup-directory "\"" ")")) (comint-send-input)
; (insert (concat "(laml-cd \"" the-laml-startup-directory "\")")) (comint-send-input)
 (insert "(laml-welcome)") (comint-send-input)
)


(defvar the-laml-startup-directory "")

(defun run-laml-interactively ()
  "Start an interactive Scheme process which in addition possess the LAML context information
of the directory, from which it has been started. In addition, it loads the LAML fundamental setup
library, the general library, the file-read library, the time library, and a HTML/XML mirror library.
Which HTML/XML mirror library to use is controlled by the variable interactive-laml-mirror-library.
If an existing inferior Lisp process and buffer exist, both are killed before a new process is started.
Runs the hook run-laml-interactively-hook."
  (interactive)

  ; kill exisisting inferior lisp process and buffer:
  (if (get-process "inferior-lisp")
      (kill-process (get-process "inferior-lisp")))
  (if (get-buffer "*inferior-lisp*")
      (kill-buffer (get-buffer "*inferior-lisp*")))

  (setq the-laml-startup-directory 
    (if (current-directory) (current-directory) (concat laml-dir "temp/")))
  (add-hook 'inferior-lisp-mode-hook 'lamlify-scheme-process)
  (let ((inferior-lisp-program scheme-exec))
    (run-lisp inferior-lisp-program))
  (run-hooks 'run-laml-interactively-hook)
)

; ---------------------------------------------------------------------------------------------------

(defvar twin-laml-buffer nil)

(defun see-laml-output ()
  "Arrange that the laml-output buffer is seen - typically after synchronous LAML processing."
  (interactive)
  (if twin-laml-buffer
      (progn
        (show-buffer (other-window 1) twin-laml-buffer)
        (other-window 1)
        (setq twin-laml-buffer nil))
      (progn
        (other-window 1) (setq twin-laml-buffer (current-buffer)) (other-window 1)
        (delete-other-windows)
        (split-window)
        (cond ((buffer-live-p (get-buffer "laml-output"))
               (show-buffer (other-window 1) "laml-output"))
              ((buffer-live-p (get-buffer "scm-output"))
               (show-buffer (other-window 1) "scm-output"))
              (t (beep) (message "Cannot find the laml/scheme output buffer")))
        (goto-char 0)
        (other-window 1)
        (message "You can reestablish an original twin buffer setup by another M-x see-laml-output. Else use C-x 1."))))


; ---------------------------------------------------------------------------------------------------
; LAML keybinding support


(defun laml-define-key (keymap key-or-key-list def &rest rest-list)
 "Bind key - or a key in key-list to def. key-or-key-list is either a string, a vector
or a list of two keys: the original and the hygienic. The rest parameter can be a single
boolean value, serving as enable (defaults to true)."

 (if (>= (length rest-list) 2) (error "laml-define-key: To many parameter passed"))
 (let ((enable (cond ((null rest-list) t)
                     (t (car rest-list)))))
  (if enable
      (let ((key (cond ((vectorp  key-or-key-list) key-or-key-list)
                       ((and (not (eq laml-keybinding-mode 'none)) (stringp key-or-key-list)) key-or-key-list)
                       ((and (listp key-or-key-list) (eq laml-keybinding-mode 'original)) (car key-or-key-list))
                       ((and (listp key-or-key-list) (eq laml-keybinding-mode 'hygienic)) (cadr key-or-key-list))
                       (t nil))))
        (if key 
            (define-key keymap key def))))))


; Customize LAML in a new frame.
(defun laml-customize ()
  (interactive)
  (let ((new-frame (make-frame)))
    (select-frame new-frame)
    (customize-group 'laml)))

; ---------------------------------------------------------------------------------------------------
; SchemeDoc support.
; More specific SchemeDoc support defined in the file
; styles/xml-in-laml/schemedoc-2/schemedoc.el

; Now defined in laml-customize.el
; (defvar scheme-documentation-style 'multi-semicolon "The documentation commenting style used for LAML SchemeDoc.
;   One of the symbols multi-semicolon or documentation-mark")

(defun schemedoc ()
  "Run LAML SchemeDoc on the Scheme source file in the current buffer.
A number of SchemeDoc processing options are extracted from the introductory comment 
of the Scheme source fil. This command saves the buffer before processing.
The variable scheme-documentation-style controls which commening style 
to use: multi-semicolon or documentation-mark."
  (interactive)
  (let* ((buf (get-buffer-create "laml-schemedoc-stuff"))
         (scheme-file-name (buffer-name (current-buffer)))
         (laml-temp-file-name "schemedoc-temp.laml")
         (cur-dir (current-directory))
         (guessed-scheme-documentation-style (guess-laml-documentation-style))
         (actual-scheme-documentation-style
           (if (eq scheme-documentation-style 'auto)
               guessed-scheme-documentation-style
               scheme-documentation-style))
       )
   (save-excursion
    (save-buffer)
    (set-buffer buf)
    (erase-buffer)
    (make-a-file-from-laml-template
      laml-temp-file-name
      cur-dir
      "schemedoc-extraction-mode"
      'laml-mode
      (list
        (list "MAN-NAME" (file-name-proper scheme-file-name))
        (list "DOC-COM-STYLE" (symbol-to-string actual-scheme-documentation-style))
        (list "SCHEME-SOURCE-FILE-NAME" scheme-file-name)    
      ))
    (write-region (point-min) (point-max) (concat cur-dir laml-temp-file-name) nil 'no-print)
    (message (concat "LAML SchemeDoc in " (symbol-to-string actual-scheme-documentation-style) " mode..."))
    (laml-sync-process-file (concat cur-dir laml-temp-file-name) 'silent)
    (delete-file (concat cur-dir laml-temp-file-name))
    (message "DONE. Consult the buffer  laml-output  to see processing details.")
   )))

(defun guess-laml-documentation-style ()
  "Return whether the current buffer is using documentation-mark or multi-semicolon style. A guess."
  (save-excursion
   (goto-char (point-min))
   (let ((r1 (search-forward ";!" nil t))
	 (r2 (search-forward "; !" nil t))
	 (r3 (search-forward ";  !" nil t))
	 (r4 (search-forward ";   !" nil t))
	 )
     (if (or r1 r2 r3 r4)
	 'documentation-mark
       'multi-semicolon))))

(defun schemedoc-help ()
 "Displays SchemeDoc help information."
 (interactive)
  (let* ((help-buffer-name "*SchemeDocHelp*")
         (help-buffer
           (if (get-buffer help-buffer-name) (get-buffer help-buffer-name) (generate-new-buffer help-buffer-name)))
        )
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file (concat laml-dir "styles/xml-in-laml/schemedoc-2/man/" "emacs-help.txt"))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))

; ---------------------------------------------------------------------------------------------------

; Misc stuff:

(defun trace-mzscheme ()
  "Insert requirements in an interactive MzScheme session that asks for error tracing."
  (interactive)
  (insert "(require (lib \"errortrace.ss\" \"errortrace\"))"))