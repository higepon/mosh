; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2005  Kurt Normark, normark@cs.auc.dk.
; 
; Execution of LAML files from Emacs.
; Top level functions: laml-process-current-buffer, laml-process-buffer,
; and laml-sync-process-current-buffer, and laml-process-file.
; 
; The first section of this files contains Scheme, platform and operating system
; dependent parts. You are supposed to augment and modify the functions start-laml-process
; and call-laml-process if you intend to port LAML.

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
; Asynchronous and synchronous LAML processing using a variety of scheme systems and platforms.
; Adpadt both of the procedures start-laml-process and call-laml-process if you port LAML to a new
; Scheme System, platform or operating system. 

(defun start-laml-process (dir file-without-extension extension output-buffer)
 "Start an asynchronous LAML process on the file characterized by the first three parameters. Show output in output-buffer.
Continue the e-lisp processing immediately after this call - does NOT wait for the termination of the started process.
Uses the global variables laml-platform, scheme-system, and scheme-exec, assigned by the LAML installation process, to
distinguish between the concrete process startup parameters."
 (let* ((file-with-extension (concat file-without-extension "." extension))  
        (file-path (concat dir file-without-extension "." extension))
        (laml-init (concat laml-dir "laml.init"))
        (load-laml-init-clause (concat "(load " "\"" laml-init "\""  ") "))
      )
  (cond ((or (eq scheme-system 'mzscheme) (eq scheme-system 'mzscheme-200) (eq scheme-system 'mzscheme-300))
           (start-process "LAML" output-buffer scheme-exec  "-v"  "-f" laml-init "-d" file-path file-with-extension dir))
 
        ((eq scheme-system 'guile)
           (let ((laml-dir-and-load-forms (concat "'" "(begin " load-laml-init-clause 
                                                       "(load " (string-it file-path) "))" "'")))
            (start-process "LAML" output-buffer "/bin/csh"  "-c" 
                (concat scheme-exec  " -c " laml-dir-and-load-forms " " file-with-extension " " dir))))

        ((eq scheme-system 'guile-old)
           (let ((laml-dir-and-load-forms (concat "'" "(begin (define laml-dir " (string-it laml-dir) ") " 
                                                       "(load " (string-it file-path) "))" "'")))
            (start-process "LAML" output-buffer "/bin/csh"  "-c" 
                (concat scheme-exec  " -c " laml-dir-and-load-forms " " file-with-extension " " dir))))

        ((eq scheme-system 'scm)
           (let ((laml-dir-definition (concat "'" load-laml-init-clause  "'")))
            (start-process "LAML" output-buffer "/bin/csh"  "-c" 
                (concat scheme-exec " -v " " -e" laml-dir-definition " -f " file-path " " file-with-extension " " dir))))

        ((eq scheme-system 'scm-old)
           (let ((laml-dir-definition (concat "'" "(define laml-dir " (string-it laml-dir) ")"  "'")))
            (start-process "LAML" output-buffer "/bin/csh"  "-c" 
                (concat scheme-exec " -v " " -e" laml-dir-definition " -f " file-path " " file-with-extension " " dir))))

        ((eq scheme-system 'sisc)
           (let ((laml-dir-define-and-forms 
                   (concat load-laml-init-clause 
                      "(define (laml-process file . args) (putprop 'argv '\\*environment-variables\\* (cons 'laml args)) (load file)))")))
            (start-process "LAML" output-buffer "/bin/sh"  "-c" 
                (concat scheme-exec " -e \"" laml-dir-define-and-forms "\" -x -c laml-process -- " file-path " " 
                        file-with-extension " " dir))))

        ((eq scheme-system 'sisc-old)
           (let ((laml-dir-define-and-forms 
                   (concat "(begin (define laml-dir \\\"" laml-dir "\\\")"
                      "(define (laml-process file . args) (putprop 'argv '\\*environment-variables\\* (cons 'laml args)) (load file)))")))
            (start-process "LAML" output-buffer "/bin/sh"  "-c" 
                (concat scheme-exec " -e \"" laml-dir-define-and-forms "\" -x -c laml-process -- " file-path " " 
                        file-with-extension " " dir))))

        ; LAML porting: Add a new case here

        (t (error "start-laml-process: non-supported Scheme system"))
  )
 )
)

(defun call-laml-process (dir file-without-extension extension output-buffer)
 "Start an synchronous LAML process on the file characterized by the first three parameters. Show output in output-buffer.
This stalls the e-lisp execution until the LAML process terminates.
Uses the global variables laml-platform, scheme-system, and scheme-exec, assigned by the LAML installation process, to
distinguish between the concrete process startup parameters."
 (let* ((file-with-extension (concat file-without-extension "." extension))
        (file-path (concat dir file-without-extension "." extension))
        (laml-init (concat laml-dir "laml.init"))
        (load-laml-init-clause (concat "(load " "\"" laml-init "\""  ") "))
      )
  (cond ((or (eq scheme-system 'mzscheme) (eq scheme-system 'mzscheme-200) (eq scheme-system 'mzscheme-300))
         (call-process scheme-exec nil output-buffer nil  "-v"  "-f" laml-init "-d" file-path file-with-extension dir))

        ((eq scheme-system 'guile) 
          (let ((laml-dir-and-load-forms (concat "'" "(begin " load-laml-init-clause 
                                                       "(load " (string-it file-path) "))" "'")))
             (call-process "/bin/csh" nil output-buffer nil "-c" 
               (concat scheme-exec " -c " laml-dir-and-load-forms  " " file-with-extension " " dir))))

        ((eq scheme-system 'guile-old) 
          (let ((laml-dir-and-load-forms (concat "'" "(begin (define laml-dir " (string-it laml-dir) ") " 
                                                       "(load " (string-it file-path) "))" "'")))
             (call-process "/bin/csh" nil output-buffer nil "-c" 
               (concat scheme-exec " -c " laml-dir-and-load-forms  " " file-with-extension " " dir))))

        ((eq scheme-system 'scm)
          (let ((laml-dir-definition (concat "'" load-laml-init-clause  "'")))
             (call-process "/bin/csh" nil output-buffer nil "-c" 
               (concat scheme-exec " -v " " -e" laml-dir-definition  " -f " file-path " "
                       file-with-extension " " dir))))

        ((eq scheme-system 'scm-old)
          (let ((laml-dir-definition (concat "'" "(define laml-dir " (string-it laml-dir) ")"  "'")))
             (call-process "/bin/csh" nil output-buffer nil "-c" 
               (concat scheme-exec " -v " " -e" laml-dir-definition  " -f " file-path " "
                       file-with-extension " " dir))))

        ((eq scheme-system 'sisc)
           (let ((laml-dir-define-and-forms 
                   (concat load-laml-init-clause 
                      "(define (laml-process file . args) (putprop 'argv '\\*environment-variables\\* (cons 'laml args)) (load file)))")))
              (call-process "/bin/csh" nil output-buffer nil "-c" 
                (concat scheme-exec " -e \"" laml-dir-define-and-forms  "\" -x -c laml-process -- " file-path " "
                       file-with-extension " " dir))))

        ((eq scheme-system 'sisc-old)
           (let ((laml-dir-define-and-forms 
                   (concat "(begin (define laml-dir \\\"" laml-dir "\\\")"
                      "(define (laml-process file . args) (putprop 'argv '\\*environment-variables\\* (cons 'laml args)) (load file)))")))
              (call-process "/bin/csh" nil output-buffer nil "-c" 
                (concat scheme-exec " -e \"" laml-dir-define-and-forms  "\" -x -c laml-process -- " file-path " "
                       file-with-extension " " dir))))

        ; LAML porting: Add a new case here 

        (t (error "call-laml-process: non-supported configuration")) 

  )
 )
)

; --------------------------------------------------------------------------------------------------------------------------------

(defvar silent-laml-processing nil 
  "If false, LAML processing will cause a buffer, laml-output, to appear at the bottom of the frame with processing information.
If true, LAML processing is is reported in a buffer laml-output which isn't shown. Go to the buffer with switch-to-buffer to consult it.")

; --------------------------------------------------------------------------------------------------------------------------------


(defun laml-process-current-buffer ()
  "Run LAML asynchronously on the file of the current buffer. Before processing, save the current buffer."
  (interactive)
  (let* ((cur-dir (current-directory))
         (file-path (concat cur-dir (current-file)))
         (buf-name-1 (buffer-name (current-buffer)))
         (buf-name
            (if (buffer-versioned buf-name-1)
                (buffer-without-version buf-name-1)
                buf-name-1))
         (filename-without-extension (file-name-proper buf-name))
         (out-buf (get-buffer-create "laml-output"))
        )
    (kill-laml-processes)
    (save-buffer)
    (message "Asynchronous LAML processing")
    (if (not silent-laml-processing) 
      (progn 
        (delete-other-windows)
        (split-window)
        (show-buffer (other-window 1) out-buf)))
    (set-buffer out-buf)
    (erase-buffer)
    (insert (concat "LAML Emacs Processing with " (symbol-to-string scheme-system))) (insert CR)
    (goto-char (point-max))

    ; Experimental - silent for now:
    (url-clipboard-transfer-and-message (file-name-directory file-path) filename-without-extension
          "" "")

    (start-laml-process cur-dir (file-name-proper buf-name) (file-name-extension buf-name) out-buf)
    (if (not silent-laml-processing) 
        (progn
          (other-window 1)
          (enlarge-window source-window-enlargement))
        (message "See LAML processing information in the buffer  laml-output  (use M-x see-laml-output)"))
   )
)


(defun laml-process-buffer (buffer)
  "Process the laml buffer by mzScheme.  Buffer may be a buffer name, or a buffer. Before processing, save the buffer."
  (interactive "BProcess which buffer: ")
  (let* ((cur-dir (current-directory))
         (buf-name (if (bufferp buffer) (buffer-name buffer) buffer))
         (the-buffer (if (bufferp buffer) buffer (get-buffer buffer)))
         (buf-name-1
            (if (buffer-versioned buf-name)
                (buffer-without-version buf-name)
                buf-name))
         (filename-without-extension (file-name-proper buf-name-1))
         (out-buf (get-buffer-create "laml-output"))
         (buf-dir (file-name-directory (buffer-file-name the-buffer)))
        )
    (kill-laml-processes)
    (set-buffer buffer)
    (save-buffer)
    (delete-other-windows)
    (split-window)
    (show-buffer (other-window 1) out-buf)
    (set-buffer out-buf)
    (erase-buffer)
    (insert (concat "LAML Emacs Processing with " (symbol-to-string scheme-system))) (insert CR)
    (start-laml-process buf-dir filename-without-extension (file-name-extension buf-name) out-buf)))


(defun laml-sync-process-current-buffer ()
  "Run mzScheme on the file in the current buffer. Wait until finished. Show the last line of the Scheme output in the minibuffer"
  (interactive)
  (let* ((cur-dir (current-directory))
         (laml-output (get-buffer-create "laml-output"))
         (file-path (concat (current-directory) (current-file)))
         (laml-init (concat laml-dir "laml.init"))
         (buf-name-1 (buffer-name (current-buffer)))
         (buf-name
             (if (buffer-versioned buf-name-1)
                 (buffer-without-version buf-name-1)
                 buf-name-1))
         (filename-without-extension (file-name-proper buf-name))
       )
    (save-buffer)
    (set-buffer laml-output)
    (erase-buffer)
    (insert (concat "LAML Emacs synchronous processing with " (symbol-to-string scheme-system))) (insert CR)
    (message (concat "Synchronous LAML processing with " (symbol-to-string scheme-system) ". Please wait..."))
    (call-laml-process cur-dir filename-without-extension (file-name-extension buf-name) laml-output)

    (laml-sync-process-end laml-output)

    (url-clipboard-transfer-and-message (file-name-directory file-path) filename-without-extension
          "DONE.  URL copied to the clipboard"
          "DONE.  See the buffer  laml-output  for details.")

 ))


(defun laml-process-file (file &optional processing-mode)
  "LAML Process a file asynchronously.  The parameter is assumed to be a full path to a LAML file.
As feedback, show the Scheme output while processing. The presentation of the processing
can be suppressed by passing the symbol silent as processing mode."
  (let* ((path (file-name-directory file))
         (filename-without-extension (file-name-proper (file-name-nondirectory file)))
         (file-extension (file-name-extension (file-name-nondirectory file)))
         (out-buf (get-buffer-create "laml-output")))
    (kill-laml-processes)

    (if (not (and processing-mode (eq processing-mode 'silent)))
        (progn
	  (delete-other-windows)
	  (split-window)
	  (show-buffer (other-window 1) out-buf)
	  (set-buffer out-buf)
	  (erase-buffer)
	  (insert (concat "LAML Emacs Processing of file " file " with " (symbol-to-string scheme-system) CR))))

    (url-clipboard-transfer-and-message (file-name-directory path) filename-without-extension
          "" "")

    (start-laml-process path filename-without-extension file-extension out-buf)))

(defun laml-sync-process-file (file &optional processing-mode)
  "LAML Process a file synchronously.  The parameter is assumed to be a full path to a LAML file.
As feedback, show the Scheme output while processing. The presentation of the processing
can be suppressed by passing the symbol silent as processing mode."
  (let* ((path (file-name-directory file))
         (filename-without-extension (file-name-proper (file-name-nondirectory file)))
         (file-extension (file-name-extension (file-name-nondirectory file)))
         (out-buf (get-buffer-create "laml-output")))
    (kill-laml-processes)

    (if (not (and processing-mode (eq processing-mode 'silent)))
        (progn
	  (delete-other-windows)
	  (split-window)
	  (show-buffer (other-window 1) out-buf)
	  (set-buffer out-buf)
	  (erase-buffer)
	  (insert (concat "LAML Emacs Processing of file " file " with " (symbol-to-string scheme-system) CR))))

    (call-laml-process path filename-without-extension file-extension out-buf)))


; ---------------------------------------------------------------------------------------------------------------
; Dired LAML execution

(defun laml-process-selected-file-from-dired ()
 "Process a single LAML file asynchronously from the selected line in dired."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil)))
   (if (and (= 1 (length selected-files)) (file-regular-p (car selected-files)))
       (let* ((fpath (car selected-files)))
          (laml-process-file fpath)
          (other-window 1)
       )
     (progn
       (beep)
       (message "Cannot process multi selection. Use M-x laml-process-selected-multiple-files-from-dired instead.")))))

(defun laml-process-unique-laml-file-from-dired ()
  "Process a single LAML file asynchronously from dired.
If a LAML file is selected, process it.
If a directory is selected in which a single LAML file exists, process that LAML file.
If there is a single LAML file in the current directory process it."
 (interactive)
 (let* ((selected-files (dired-get-marked-files nil))
        (all-files (directory-files (current-directory)))
        (all-laml-files 
         (filter
          (function (lambda (f) (member (file-name-extension f) (list "laml" "leno" "chord")))) all-files))
       )
   (cond 
         ((and (= 1 (length selected-files))                                           ; LAML file explicitly selected
               (file-regular-p (car selected-files))
               (member (file-name-extension (car selected-files)) (list "laml" "leno" "chord"))
          )
         (let* ((fpath (car selected-files)))
            (laml-process-file fpath)
            (other-window 1)
            ))

         ((and (= 1 (length selected-files))                                          ; A directory is selected
               (file-directory-p (car selected-files)))                               ; Look for single LAML file in ít
           (let* ((fpath (car selected-files))
                  (files-in-selected-dir (directory-files fpath))
                  (all-laml-files 
                    (filter (function (lambda (f) (member (file-name-extension f) (list "laml" "leno" "chord")))) 
                            files-in-selected-dir))
                 )
             (if (= 1 (length all-laml-files))
                 (progn
                   (laml-process-file (concat fpath "/" (car all-laml-files)))
                   (other-window 1))
                 (message "There is no unique LAML file inside the selected directory"))))

         ((and (= 1 (length all-laml-files)) (file-regular-p (car all-laml-files)))   ; Only one LAML file in dir
          (let* ((fpath (concat (current-directory) (car all-laml-files))))
            (laml-process-file fpath)
            (other-window 1)
            ))

         (t
          (progn
            (beep)
            (message "There is not a single unique laml file in this directory. Nothing done.")))))) 

(defun laml-process-selected-multiple-files-from-dired ()
 "Process a number er LAML files synchronously from the selected lines in dired."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil))
       (n 0)
      )
   (if (and (>= (length selected-files) 1))
       (progn
         (if (get-buffer "laml-output") (erase-named-buffer (get-buffer "laml-output")))
         (while selected-files
	   (let* ((fpath (car selected-files)))
	     (if (file-regular-p fpath)
		 (progn 
		   (message (concat "LAML processing " fpath))
		   (laml-sync-process-file fpath 'silent)
		   (setq n (+ n 1))
		   )
	       (message (concat "Cannot process " fpath)))
	     (setq selected-files (cdr selected-files))
	     ))
         ; (laml-sync-process-end (get-buffer "laml-output"))
       )
     (progn
       (beep)
       (message "...")))
   (message (concat "LAML processing DONE using " (symbol-to-string scheme-system) "." " Processed " (int-to-string n) " files."))))

; ---------------------------------------------------------------------------------------------------------------
; Auxiliary stuff.

(defun url-clipboard-transfer-and-message (initial-path filename-without-extension message-url-copied else-message)
  "Copy an appropriate URL to clipboard when saving a HTML file with initial-path and filename-without-extension.
Give the message message-url-copied or else-message."
  (let* ((fp-html (concat initial-path filename-without-extension "." "html"))
         (fp-svg (concat initial-path filename-without-extension "." "svg"))
         (abs-file-path-to-url-html (absolute-file-path-to-url fp-html))
         (abs-file-path-to-url-svg (absolute-file-path-to-url fp-svg)))
      (cond ((and (file-exists-p fp-html) abs-file-path-to-url-html)
	       (progn
		 (kill-new abs-file-path-to-url-html) ; transfer url to clip board
		 (if (> (length message-url-copied) 0) (message message-url-copied))))
            ((and (file-exists-p fp-svg) abs-file-path-to-url-svg)
	       (progn
		 (kill-new abs-file-path-to-url-svg) ; transfer url to clip board
		 (if (> (length message-url-copied) 0) (message message-url-copied))))
            (t
              (if (> (length else-message) 0) (message else-message))))))


(defun laml-sync-process-end (laml-output-buffer)
 (if (not (eq system-type 'windows-nt))
     (let* ((tooltip-frame-parameters '((left . 10) ; dynamic binding - shadowing more global variable
					(top . 10)
					(name . "tooltip")
					(internal-border-width . 5)
					(border-width . 1)))
	    (tooltip-lines (cdr x-max-tooltip-size))
            (buffer-txt (buffer-string-of-buffer laml-output-buffer))
            (lines-in-buffer-txt (number-of-lines-in-string buffer-txt))
	    (txt (if (<= lines-in-buffer-txt tooltip-lines) 
                     buffer-txt
                     (string-line-range buffer-txt (- lines-in-buffer-txt tooltip-lines) lines-in-buffer-txt)))
	   )
       (tooltip-show txt))))

; ---------------------------------------------------------------------------------------------------------------

