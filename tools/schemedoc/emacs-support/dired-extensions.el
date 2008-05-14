; Dired extensions.
; File manipulations based on a split window layout, where files are copied from the dired in 
; Emacs window to another.
 
; Not used by LAML. Not loaded by LAML emacs support. Just conveniently bundled with LAML.

; Extensions to dired that allow us to copy files from on dired directory to another
; to copy directories recursively, and to delete directories recursively.
; You should be very careful when using these powerful procedures. Few questions are asked -
; the work is just done.

(defun copy-files-to-other-window-directory ()
  "From dired, copy the selected files and directories to other windows current buffer.
Selected directories are copied recursively, but the target directory is not deleted first.
Deleting directories can be done with M-x delete-files-from-dired.
This procedure assumes you are have splitted you current frame (window) in two sub-windows,
one of which is running dired. Copy the selected files from the dired window to the other buffer's
directory, even the target files exist."
  (interactive)
  (save-excursion
    (let ((selected-files (dired-get-marked-files nil)))
      (dired-unmark-all-marks)
      (other-window 1)
      (let ((other-windows-dir (current-directory)))
        (if (and other-windows-dir (> (length selected-files) 0))
            (progn
             (copy-files selected-files other-windows-dir)
             (if (eq major-mode 'dired-mode) (revert-buffer))
             (other-window 1)
             (message "DONE"))

            (message "You must be in a dired context, and the other window must have a current-directory. Nothing done."))))))


(defun delete-files-from-dired (non-confirm-p)
 "From dired, delete selected files and directories recursively. Notice that 
you should not use the delete mark, but the normal mark (*) to delete with this command.
The directories . and .. cannot be deleted by this procedure.
In case nothing is marked, the file or directory under point is deleted!
The parameter, non-confirm-p, controls if the deletions are to confirmed before executed.
Without prefix argument, using C-u, non-confirm-p nil.
With prefix argument, using C-u, non-confirm-p becomes non-nil.
"
  (interactive "P")
  (save-excursion
    (let* ((selected-files (dired-get-marked-files nil))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files))
          )
      (if (not non-confirm-p) ; interactive default
        (if (yes-or-no-p 
	     (if (= 1 (length selected-files))
		 (let* ((fpath (car selected-files))
			(fname (file-name-nondirectory fpath))
			(fprop (file-name-proper fname))
			(fext (file-name-extension fpath))
			(faggr (concat fprop (if fext (concat "." fext) ""))))
		   (concat "Delete file or directory " faggr "? "))
                 (let* ((mes (measure-dired-selection))
                        (top-dir-count (car mes))
                        (file-total-count (cdr mes))
                       )
                    (cond ((= 0 top-dir-count) 
                            (concat "Delete a total of " (number-to-string file-total-count) " files "))
                          ((= 1 top-dir-count) 
                            (concat "Delete a total of " (number-to-string file-total-count) " files, including the files in one directory? "))
                          (t 
                            (concat "Delete a total of " (number-to-string file-total-count) " files, including the files in " (number-to-string top-dir-count) " directories? ")
                   )))))
            (progn
	     (mapcar
	      (lambda(f)
		(cond ((file-directory-p f) 
		       (delete-directory-recursively f))
		      ((file-regular-p f)
		       (delete-file f)
		       (message "Deleted file: " f))
		      (t nil)))
	      selected-files-1)
             (revert-buffer))
	  (message "Nothing deleted"))
        (progn
         (mapcar
	  (lambda(f)
	    (cond ((file-directory-p f) 
		   (delete-directory-recursively f))
		  ((file-regular-p f)
		   (delete-file f)
		   (message "Deleted file: " f))
		  (t nil)))
	  selected-files-1)
         (revert-buffer))
      ))))

(defun move-files-to-other-window-directory ()
  "From dired, move the selected files and directories to other windows current buffer.
Moving a directory is probably problematic. 
This procedure assumes you are have splitted you current frame (window) in two sub-windows,
one of which is running dired. Move the selected files from the dired window to the other buffer's
directory, even the target files exist."
  (interactive)
  (save-excursion
    (let ((selected-files (dired-get-marked-files nil)))
      (dired-unmark-all-marks)
      (other-window 1)
      (let ((other-windows-dir (current-directory)))
        (if (and other-windows-dir (> (length selected-files) 0))
            (progn
             (move-files selected-files other-windows-dir)
             (if (eq major-mode 'dired-mode) (revert-buffer))
             (other-window 1)
             (revert-buffer)
             (message "DONE"))

            (message "You must be in a dired context, and the other window must have a current-directory. Nothing done."))))))

; Return a cons pair of the number of (top level) directories and the total number of files
; in the current dired selection
(defun measure-dired-selection ()
  (save-excursion
    (let* ((selected-files (dired-get-marked-files nil))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files))
           (dir-list (filter (function file-directory-p) selected-files-1))
           (dir-list-1 (mapcar (lambda (x) (concat x "/")) dir-list))
           (file-list (filter (function file-regular-p) selected-files-1))
          )
      (cons 
       (length dir-list)
       (+ (length file-list)
          (accumulate-right (function +) 0 (mapcar (function count-files) dir-list-1)))))))


(defun count-files (dir)
  (let* ((files (list-of-files dir))
         (dirs (list-of-directories dir))
         (full-path-dirs (mapcar (lambda (d) (concat dir d "/")) dirs))
        )
   (+ (length files) 
      (accumulate-right (function +) 0 (mapcar (function count-files) full-path-dirs)))))

  

(defun move-files (file-path-list target-dir)
  (mapcar
   (function 
    (lambda (f)
	(cond ((file-directory-p f)   ; works? 
               (let* (
                      (name (file-name-nondirectory f))
                      (initial-path (file-name-directory f))
                     )
	       (rename-file f target-dir t)))

	      ((file-regular-p f)
               (let* (
                      (name (file-name-proper (file-name-nondirectory f)))
          	      (ext (file-name-extension f))
	              (name-ext (concat name (if ext (concat "." ext) "")))
                      (initial-path (file-name-directory f))
                     )
	       (rename-file f (concat target-dir name-ext) t)
	       (message (concat f " => " (concat target-dir name-ext)))))

	      (t nil))
	))
   file-path-list))


; Copy all files in file-path-list (list of full paths) to target dir
(defun copy-files (file-path-list target-dir)
  (mapcar
   (function 
    (lambda (f)
	(cond ((file-directory-p f) 
               (let* (
                      (name (file-name-nondirectory f))
                      (initial-path (file-name-directory f))
                     )
	       (copy-directory-recursively initial-path name target-dir)))

	      ((file-regular-p f)
               (let* (
                      (name (file-name-proper (file-name-nondirectory f)))
          	      (ext (file-name-extension f))
	              (name-ext (concat name (if ext (concat "." ext) "")))
                      (initial-path (file-name-directory f))
                     )
	       (message (concat f " => " (concat target-dir name-ext)))
	       (copy-file f (concat target-dir name-ext) t t)))

	      (t nil))
	))
   file-path-list))



; copy directory d in in-dir to to-dir, and recursively, all files too.
; indir/d => to-dir/d
(defun copy-directory-recursively (in-dir d to-dir)
  (if (not (overlapping-paths-p (concat in-dir d "/") to-dir))
      (progn
        (if (not (file-exists-p (concat to-dir d))) ; directory in-dir/d does not exist
	    (make-directory (concat to-dir d))) ; makes d in to-dir
	(let* ((files-in-d (append (list-of-files (concat in-dir d "/")) (list-of-directories (concat in-dir d "/"))))
	       (file-paths-in-d 
		(mapcar (lambda (f) (concat in-dir d "/" f)) files-in-d))
	       )
	  (copy-files file-paths-in-d (concat to-dir d "/"))
	  ))
       (error "You cannot copy a directory into itself or one of its subdirectories")))

; Is dir2 located in dir1?
; Should perhaps be conservative with respect to upper/lower case of directories (on windows, at least)
(defun overlapping-paths-p (dir1 dir2)
  (if (>= (length dir2) (length dir1))
      (equal (substring dir2 0 (length dir1)) dir1)
      nil))

(defun delete-directory-recursively (dir)
 (let* ((dir-lgt (length dir))
        (dir1 (if (equal (substring dir (- dir-lgt 1) dir-lgt) "/") dir (concat dir "/")))
        (dir-files (list-of-files dir1))
        (dir-dirs (list-of-directories dir1))
       )
   (mapcar (lambda (f) 
             (message "Deleting file: " f)
             (delete-file (concat dir1 f)))
           dir-files)
   (mapcar (lambda (d) (delete-directory-recursively (concat dir1 d "/"))) dir-dirs)
   (message (concat "Deleting " dir1))
   (delete-directory dir1)))
  
   
(defun list-of-directories (dir)
 "Return a list of directories in DIR. Each entry in the
list is a string. The list does not include the current directory
and the parent directory."
 (filter (function (lambda (x) (not (or (equal x ".") (equal x "..")))))
  (filter
   (function (lambda (x)
	       (file-directory-p (concat dir x))))
   (directory-files dir))))

(defun list-of-files (dir)
  "Return a list of regular files in DIR. Each entry in the
list is a string."
  (filter
   (function (lambda (x)
	       (file-regular-p (concat dir x))))
   (directory-files dir)))


(defun unzip-file-from-dired ()
 "Unzip the selected file from a dired window.
Only a single zip file can be unzipped at a time."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil)))
   (if (= 1 (length selected-files))
       (let* ((fpath (car selected-files))
	      (fname (file-name-nondirectory fpath))
	      (in-dir (file-name-directory fpath))
	      (fprop (file-name-proper fname))
	      (fext (file-name-extension fpath)))
	 (if (equal fext "zip")
	     (progn
               (message "Unzipping ...")
               (call-process "/bin/sh" nil nil t "-c" 
			   (concat "cd " in-dir ";" "unzip " fname))
               (revert-buffer)
               (message "DONE")
              )
	   (progn
	     (beep)
	     (message "You can only unzip a file with extension zip"))))
     (progn
       (beep)
       (message "You can only unzip a single file at a time")))))


(defun zip-directory-from-dired ()
 "Zip the selected directory from a dired window. Put the zip file in the current directory.
Only a single directory can be zipped at a time."
 (interactive)
 (let ((selected-files (dired-get-marked-files nil)))
   (if (and (= 1 (length selected-files)) (file-directory-p (car selected-files)))
       (let* ((fpath (car selected-files))
	      (dir (file-name-nondirectory fpath))
	      (in-dir (file-name-directory fpath))
             )
          (message "Zipping ...")
          (zip in-dir dir)
          (revert-buffer)
          (message "DONE")
       )
     (progn
       (beep)
       (message "You can only zip a single directory at a time")))))



(defun files-satisfying (dir pred)
  "Return a list of those file names (full path) in dir that satisfy pred.
pred is applied on the full path name.
dir is a slash terminated full path to a directory."
  (let ((file-list (list-of-files dir))
        (dir-list (list-of-directories dir)))
    (append
      (filter pred (mapcar (lambda (fn) (concat dir fn) ) file-list))
      (accumulate-right (function append) nil 
       (mapcar 
	(function
	 (lambda (subdir) (files-satisfying (concat dir subdir "/") pred)))
	dir-list)))))


(defun print-files-from-dired ()
 "From dired, print selected files. Notice that 
you should the normal mark (*) to print with this command.
Directories, including . and .., cannot be printed by this procedure.
In case nothing is marked, the file or directory under point is printed.
This function is specific to cs.aau.dk, and it depends on Unix and a2ps.
It also depends on Emacs Lisp stuff outside the LAML distribution.
"
  (interactive)
  (save-excursion
    (let* ((printer
   	     (read-from-minibuffer "Which printer (c12a e12a e11a e21b e21c e22a c12a) " "e42a"))
           (selected-files (dired-get-marked-files nil))
           (selected-files-non-dir (filter (lambda (f) (not (file-directory-p f))) selected-files))
           (selected-files-1 
              (filter 
                (function (lambda (x) 
                 (let ((y (file-name-nondirectory x)))
                   (not (or (equal y ".") (equal y ".."))))))
                selected-files-non-dir))
          )
      (mapcar
       (function 
        (lambda (f) 
           (prc-enscript-file f 'a2ps printer)))
        selected-files-1))))


