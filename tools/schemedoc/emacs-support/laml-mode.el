; A LAML mode used to distinguish LAML buffers from other buffers.
; Contains the mode stuff and a number of useful LAML related Emacs commands.

; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2005  Kurt Normark, normark@cs.auc.dk.
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


(define-derived-mode 
        laml-mode scheme-mode "Laml" 
  "A LAML Emacs mode mode, which binds some keys (see below) to a useful LAML meaning. 
LAML means 'Lisp Abstracted Markup Language'. LAML makes HTML and XML languages available as 
Scheme functions. 

The general LAML key bindings are defined in the file emacs-support/laml-key-menu-bindings.el
of your LAML distribution.

Used in hygienic mode, the most important LAML mode key bindings are

 C-c C-o       Asynchronous LAML processing
 C-c C-x C-o   Synchronous LAML processing
 C-c C-x C-e   Embeding of selected string in a Scheme function and accompanying string splitting.
 C-c C-r       Unembed - opposite command of embed.
 C-c C-q       Split string.
 C-c C-a       Unsplit string - opposite command of split.
 C-c C-n       Nest form.
 C-x C-m       Unnest form -  opposite command of nest.

Using in original mode, the bindings are

 C-o           Asynchronous LAML processing
 C-x C-o       Synchronous LAML processing
 C-x C-e       Embeding of selected string in a Scheme function and accompanying string splitting.
 C-x C-r       Unembed - opposite command of embed.
 C-x C-q       Split string.
 C-x C-a       Unsplit string - opposite command of split.
 C-x C-n       Nest form.
 C-x C-m       Unnest form -  opposite command of nest.

The choice between hygienic and original mode is made at LAML installation time, in the 
file laml-config/configuration.

You should notice the Laml menu in Emacs, which is available in LAML mode.
Also notice Tools > Laml menu, which is always present.

Laml mode is derived from Scheme mode, and Scheme font locking is in effect.

Use M-x laml-customize to customize LAML mode. 

Runs laml-mode-hook for extension purposes."

  (font-lock-mode t)

)

(defvar laml-elisp-dir (concat laml-dir "emacs-support/") "The directory which contains this file. Ends in a slash")

(defvar laml-pp-helpful t 
  "Show useful role names of constituents in selected LAML templates. 

If t always pretty print role strings in templates. If nil, just print empty strings for some template elements
LAML templates are activated by M-x insert-... in the various LAML styles. In LENO, use M-x leno-insert-...")



; ---------------------------------------------------------------------------------------------------
; General template support for emacs:


(defvar laml-template-dir (concat laml-elisp-dir "templates/")
   "The directory path to the directory, in which the templates (tpl as well as accompanying el) files are stored.")

(defun laml-insert-template (tmpl-name &optional template-dir)
  "Inserts a template from the template collection in template-dir. If template-dir is not provided, take template from the standard LAML template dir."
  (interactive 
    (list
      (completing-read
       "Template name: "
       (mapcar 'laml-tmpl-transform-name
               (filter 'laml-tmpl-tpl-filter
                       (file-name-all-completions "" laml-template-dir)))
             nil t nil)))
  (let ((template-dir-1 (if template-dir template-dir laml-template-dir)))
    (insert-file (concat template-dir-1  tmpl-name ".tpl"))
    (let ((el-file (concat template-dir-1 tmpl-name ".el")))
      (if (file-exists-p el-file)
	  (load-file el-file)))))

(defun laml-get-template (tmpl-name)
  "Return the template string of tmpl-name"
  (let ((tmpl-path (concat laml-template-dir tmpl-name ".tpl")))
    (read-text-file tmpl-path)))  ; not implemented


(defun insert-laml-template ()
    "Return a template from the template collection in the directory
templates. Returns a string"
  (interactive)
  (call-interactively 'laml-insert-template))

;; Functions that help generate a completion list of template names:

(defun laml-tmpl-transform-name (n)
  "Function used for the production of the template name completion list"
  (cons (substring n 0 (- (length n) 4)) nil))

(defun laml-tmpl-tpl-filter (n)
  (equal (substring n (- (length n) 4)) ".tpl"))


; ---------------------------------------------------------------------------------------------------
; Template insertion
  
(defun insert-lisp-form (form indentation)
  (setq indent-tabs-mode nil)
  (ensure-indentation 0)
  (insert (concat (make-string indentation 32) form))
  (backward-sexp 1)
  (forward-sexp 1) (beginning-of-line)
  (ensure-indentation indentation)
  (end-of-line)
  (backward-sexp 1)
  (indent-sexp)
)

; old, but still used
(defun insert-lisp-form-0 (form indentation)
  (insert-lisp-form (laml-transliterate form) indentation))


; ---------------------------------------------------------------------------------------------------
; Transliteration stuff:

(defvar empty-string "\"\"")

(defun laml-transliterate (form)
  "Return a copy with various transliterations" 
  (let* ((res1 (string-to-list form))
         (res2 (if (eq (car res1) 40)
                   (cons 40 (transliterate-list (cdr res1) 40 (concat CR "(")))  ; avoid CR before first start parenthesis
                   (transliterate-list res1 40 (concat CR "("))))
         (res3 (transliterate-list res2 41 (concat ")" )))
         (res4 (transliterate-list res3 ?E (concat empty-string CR )))
        )
    (list-to-string res4)))


(defun transliterate-list (list char x)
  "List is a list of characters (integers). Replaces occurences of char with x.
The parameter x can be a string or a character"
  (let ((list1 list)
        (res nil))
    (while list1 
      (setq res
        (cons 
          (if (eq (car list1) char)
              x
              (car list1))
          res))
      (setq list1 (cdr list1)))
    (reverse res)))


(defun charlist-to-string (lst)
  "The opposite of string-to-list"
  (let ((str (make-string (length lst) 0))
        (lst1 lst)
        (i 0))
    (while lst1 
       (aset str i (car lst1))
       (setq i (+ i 1))
       (setq lst1 (cdr lst1)))
    str))

; (defun list-to-string (lst)
;   "Convert a list of mixed chars and strings to a string"
;   (let ((str (make-string (length-char-string-list lst) 0))
;         (lst1 lst)
;         (i 0))
;     (while lst1 
;        (aset-char-string str i (car lst1))
;        (setq i (+ i (length-char-string (car lst1))))
;        (setq lst1 (cdr lst1)))
;     str))

(defun list-to-string (lst &optional sep)
 "Convert a list of mixed chars and strings to a string. Separate elements by the string sep."
  (let* ((sep1 (if sep sep ""))
         (lgt-lst (length lst))
         (lgt-sep1 (length sep1)))
    (let ((str (make-string (+ (* (max (- lgt-lst 1) 0) lgt-sep1) (length-char-string-list lst)) 0))
          (lst1 lst)
          (i 0) (j 0))
      (while lst1 
        (aset-char-string str i (car lst1))
        (setq i (+ i (length-char-string (car lst1)))) ; advance i by element length
        (if (< j (- lgt-lst 1)) 
            (progn (aset-char-string str i sep1) ; insert separator
                   (setq i (+ i lgt-sep1))))     ; advance i by separator length
        (setq j (+ j 1)) 
        (setq lst1 (cdr lst1)))
      str)))


(defun string-to-list (str)
  "Return a list of characters, i.e., integers from the string STR"
 (let ((res nil)
       (strg str))
   (while (> (length strg)  0)
     (progn
        (setq res (cons (string-to-char strg) res))
        (setq strg (substring strg 1))
        ))
   (reverse res)))


(defun length-char-string-list (lst)
  "Calculate the length of lst. Chars count 1, strings count their lengths"
 (let ((res 0)
       (lst1 lst))
  (while lst1
    (setq res (+ res (length-char-string (car lst1))))
    (setq lst1 (cdr lst1)))
  res))

(defun length-char-string (x)
  "Return the length of x. Chars count 1, strings count their lengths"
  (cond ((integerp x) 1)
        ((stringp x) (length x))))

(defun aset-char-string (str i x)
  "like aset, but x may also be a string, in which case all chars in x are inserted"
  (cond ((integerp x) (aset str i x))
        ((stringp x) (aset-string str i x 0))))

(defun aset-string (str i str1 j)
  "Assign destructively str1 into position i in str. j is a helping, indexing parameter"
  (if (= j (length str1))
      nil  ; result in str
      (progn 
        (aset str i (aref str1 j))
        (aset-string str (+ i 1) str1 (+ j 1)))))
          


; ---------------------------------------------------------------------------------------------------
; Indentation stuff:

(defun determine-indentation ()
  (let ((ci (real-indentation-of-string (this-line))))
    (if (> ci 0)
        ci
        (indentation-of-previous-line))))

   
(defun indentation-of-previous-line ()
  "Return the indentation of the previous line. Here previous line means the previous
real line. Empty lines are not taken into consideration. In case no previous line exist, return 0"
  (save-excursion
    (beginning-of-line)
    (let ((prev (prev-line)))
      (if prev
          (cond ((space-stringp prev) (progn (previous-line 1) (indentation-of-previous-line)))
                (t (indentation-of-string prev)))
          0))))

(defun real-indentation-of-string (str)
  (cond ((equal str "") 0)
        ((space-stringp str) (length str))
        (t (let ((i 0))
             (while (eq (aref str i) 32)
               (setq i (+ i 1)))
             i))))

(defun indentation-of-string (str)
  "Return the number of leadning spaces of str. 
If string is empty, or if it consists soly of spaces, return 0"
 (if (or (equal str "") (space-stringp str))
     0
     (let ((i 0))
       (while (eq (aref str i) 32)
          (setq i (+ i 1)))
       i)))

(defun space-stringp (str)
  "Is str full of spaces"
  (or (= 0 (length str)) 
      (equal str (make-string (length str) 32))
  ))

(defun prev-line ()
 (save-excursion
  (let ((p1 (point)))
    (previous-line 1)
    (if (eq p1 (point))
        nil
        (this-line)))))

(defun this-line ()
  (save-excursion 
    (beginning-of-line)
    (let ((p1 (point)))
      (end-of-line)
      (buffer-substring p1 (point)))))    
     
(defun ensure-indentation (i)
  "Enforce that the current line is indented i positions."
  (save-excursion
   (let ((ci (real-indentation-of-string (this-line))))
      (if (not (= i ci))
          (progn
             (beginning-of-line)
             (delete-char ci)
             (insert-char 32 i))))))


; ---------------------------------------------------------------------------------------------------
; File making from template.
; (Originally located in elucidator.el)

(defun make-a-file-from-laml-template (file dir template-name emacs-mode replacements &optional template-dir)
  "Make the FILE in DIR. Then insert the LAML template TEMPLATE-NAME, either from the default LAML template dir or from TEMPLATE-DIR if provided.
Finally carry out the list of REPLACEMENTS. Return the files buffer. "
  (let ((template-dir-1 (if template-dir template-dir laml-template-dir))
        (new-buffer (create-file-buffer (concat dir file))))
    (set-buffer new-buffer)
    (eval (list emacs-mode)) ; set the mode
    (laml-insert-template template-name template-dir-1)
    (do-perform-replacements replacements)
    (write-file (concat dir file))
    new-buffer))

(defun do-perform-replacements (replacement-list)
  (mapcar
    (function do-perform-replacement)
    replacement-list))

(defun do-perform-replacement (repl)
  (let ((from-string (car repl))
        (to-string (cadr repl)))
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match to-string t t))))



; ---------------------------------------------------------------------------------------------------

; String splitting stuff:

(defvar QUOTE-CHAR 34)
(defvar SPACE-CHAR 32)
(defvar BACKWARD-SLASH 92)
(defvar QUOTE-STRING (char-to-string QUOTE-CHAR))

(defun goto-start-of-string ()
  "Assumes that the point is inside a string (not point at a quote).
Move the point to the start of the string, pointing at the quote character"
  (while (not (eq (char-under-point) QUOTE-CHAR))
         (backward-char 1)))

(defun goto-end-of-string ()
  "Assumes that the point is inside a string (not point at a quote).
Move the point to the end of the string, poining at the quote character"
  (while (not (eq (char-under-point) QUOTE-CHAR))
         (forward-char 1)))

(defun char-under-point (&optional n)
  "Return the character under point. If n is supplied, it must be a number.
   In that case n is added to point before the character is returned. In that
   way we can access chacters before (-1) or after (1) point too."
  (let* ((n1 (if n n 0))
         (pt1 (+ (point) n1)))
    (string-to-char (buffer-substring pt1 (+ 1 pt1)))))

(defun word-delimitor-scheme ()
  "Return a list of two numbers: the start and the end positions of the Scheme name under point"
 (save-excursion
  (let ((rb (progn (goto-start-of-scheme-word)
                 (point)))
        (re (progn (goto-end-of-scheme-word)
                 (point))))
    (if (and rb re) (list rb re)))))

(defun scheme-name-under-point ()
  (let ((wd (word-delimitor-scheme)))
    (buffer-substring (car wd) (+ 1  (cadr wd)))))

; remove all spaces in the neighborhood of the location p
; Return the number of spaces removed
(defun space-remove (p)
 (let ((i 0))
  (save-excursion 
   (goto-char p)
   ; first go back without deleting spaces:
   (if (looking-at " ") (progn (while (looking-at " ") (backward-char 1)) (forward-char 1)))

   ; Now delete and count in the variable i:
   (while (looking-at " ") (progn (delete-char 1) (setq i (+ i 1)))))
  i))


(defun split-string-into-three (&rest point-list)
  "Assume that point is within a string and that point and mark defines a selection. Split the current string into
   three strings: The section before the selection, the selection, and the section after. Leave point at the newly inserted start
   quote of the selected string.
   If point-list of provided as parameters, it must be a list of two buffer possitions p1 and p2.
  "
 (let ((qs (char-to-string QUOTE-CHAR)))
  (let ((p1 (if point-list (car point-list) (min (point) (mark))))
        (p2 (if point-list (cadr point-list) (max (point) (mark)))))
    
    ; remove a space to the right
    (if (not laml-single-string-approach) (space-remove p2))

    ; remove a space to the left
    (if (not laml-single-string-approach)
        (if (not (at-indentation p1))
          (let ((count (space-remove (- p1 1))))
            (setq p1 (- p1 count)) (setq p2 (- p2 count)))))
    
    (goto-char p2) (insert (concat qs " " qs))
    (goto-char p1) (insert (concat qs " " qs))
    (goto-char (+ 2 p1))
)))




(defun at-indentation (p)
 "Is possition p an indentation possition."
 (save-excursion
  (goto-char p)
  (backward-char 1)
  (if (looking-at-chars (point) (list 32 9))
      (progn
        (while (looking-at-chars (point) (list 32 9)) (backward-char 1))
        (looking-at-chars (point) (list 13 10)))
      nil)))


(defun split-string-into-two ()
  "Assume that point is within a string. Split the current string into
   two strings: The section before the selection and the section after. Leave point between the two strings."
 (let ((qs (char-to-string QUOTE-CHAR)))
  (if (not laml-single-string-approach) (space-remove (point)))
  (let* ((p1 (point))
        )

    (goto-char p1) (insert (concat qs))
    (goto-char p1) (insert (concat qs " "))
)))


; ---------------------------------------------------------------------------------------------------------
; Generally useful LAML commands

(defvar previous-nesting "" "The previously typed nesting name - for convenient repetition of nestings.")
(defvar laml-nest-tag-list nil )
(defvar nest-require-match nil "A list of tag names (symbols) in which nesting makes sense")


(defun nest ()
  "Nest the form(s) designated by the point into a call of function-name. Function-name is a symbol.
Depends on the major and minor modes.
In case point is at a start parenthesis, the form started at this position is nested.
In case point is just after an end parenthesis, the current selection is nested.
When called interactively you are prompted for a the function-name symbol in the minibuffer.
Depends on the variables previous-nesting, laml-nest-tag-list, nest-require-match, and nest-embed-attribute-insertion-map."
  (interactive)
  (cond ((and (is-provided 'scheme-elucidator-support) elucidator-mode (in-documentation-buffer-p))
           (let (
                 (nest-require-match nil)
                 (laml-nest-tag-list '(strong-prog-ref weak-prog-ref doc-ref source-marker))
                 (nest-embed-attribute-insertion-map '(
                                                       (weak-prog-ref file vers name)
                                                       (strong-prog-ref file vers name)
                                                       (doc-ref name)
                                                       (source-marker name)
                                                       )
                                                     )
                )
              (call-interactively 'internal-nest)))
        ((and (is-provided 'laml-chords-support) (eq major-mode 'chords-mode))
           (let (
                 (previous-nesting "")
                 (nest-require-match t)
                 (laml-nest-tag-list supported-chords)
                 (nest-embed-attribute-insertion-map nil)
                )
              (call-interactively 'internal-nest)))
        (t (call-interactively 'internal-nest))))  
  

(defun internal-nest (function-name)
  "An internal version of the function nest"
  (interactive 
    (list (string-to-symbol (completing-read "Nest into which form: " 
                               (trivial-alist (mapcar (function symbol-to-string) laml-nest-tag-list))
                               nil nest-require-match previous-nesting))
          ))
  (if (just-after-form-p)
        (let* ((pt (point))
               (mk (mark)))
          (raw-nest-multiple-forms function-name pt mk))
        (raw-nest function-name)) 
  (setq previous-nesting (symbol-to-string function-name)))

; is point just after a parenthesized form?
(defun just-after-form-p ()
  (if (looking-at "(")
      nil
      (save-excursion
        (backward-char 1)
        (looking-at ")"))))

; Nest the form under point, designated by the start parenthesis, 
; in a form prefixed with the symbol function-name
(defun raw-nest (function-name)
  (insert "(") 
  (insert (symbol-to-string function-name))
  (insert " ")
  (insert-nesting-attributes function-name)
  (forward-sexp 1)
  (insert ")")
  (backward-sexp 1))

; The point is located just after position last-pt. This is supposed to be just after an end par.
; Nest the selection in between first-pt and last-pt in af formed prefixed with the symbol function-name.
(defun raw-nest-multiple-forms (function-name last-pt first-pt)
  (insert ")") 
  (goto-char first-pt)
  (insert "(") 
  (insert (symbol-to-string function-name))
  (insert " ")
  (insert-nesting-attributes function-name)
  (backward-up-list 1))

; A list of attribution insertion entries. An attribution insertion entry
; maps an element name to a list of attributes, which will be inserted when
; the element is used in nesting and embedding contexts. 
(defvar nest-embed-attribute-insertion-map
 '(
    (a href)
  ))

(defun insert-nesting-attributes (function-name)
 (let ((attribute-insertion-attributes (linear-search nest-embed-attribute-insertion-map 
                                                (function (lambda (el) (eq (car el) function-name)))
                                                (function cdr))))
  (mapcar
    (lambda (attr)
      (insert (concat "'" (symbol-to-string attr) " \"\" ")))
    attribute-insertion-attributes)))


(defun unnest ()
  "Unnest the form designated by the point. Point must be at a start or end parenthesis character.
   Leaves point just before the first constituent of the unnested form."
  (interactive)
  (if (= (char-under-point) 41)  ; end parenthesis
      (backward-up-list 1))
  (if (= (char-under-point) 40)  ; start parenthesis
      (let ((p1 (point))
            (p2 nil)
           )
        (forward-sexp 1)
        (setq p2 (point))
        (delete-backward-char 1) ; delete end parenthesis
        (goto-char p1)
        (delete-char 1)  ; delete start parenthesis
        (kill-sexp 1)    ; delete function name constituent
        (while (looking-at " ") (delete-char 1)) ; delete white spaces following the function name
        (while (< (point) p2)
          (progn
           (if (looking-at-attribute-name) 
               (progn (kill-attribute-name-and-value) (while (looking-at " ") (delete-char 1)))
               (progn (forward-sexp 1) (while (looking-at " ") (delete-char 1))))
           (insert " ") ; insert exactly one space, and pass point over it.
           ))
        (goto-char p1)
      )
  )
)

(defun looking-at-attribute-name()
  (= (char-under-point) ?'))

(defun kill-attribute-name-and-value ()
  (kill-sexp 2)  
)



;(defvar laml-single-string-approach nil 
;  "A boolean variable which defines whether we work with LAML forms which expect a single string as argument.
;The alternative is functions which accept an arbitrary list of strings as arguments.")

(defvar smart-word-determination t 
  "With smart word determination a word is implicitely selected if the point is at the first character of a word.

If you do not use smart word determination you must explicitly select a string to embed. The variable is important for the embed command.")


(defun ensure-concatenation-context ()
  "Point is assumed to be within a string. Make sure that the string is within a direct concatenation context.
If necessary, insert a con form. Con is an alias of string-append in LAML."
  (save-excursion
    (let ((p-cur (point))
          (p-stringstart nil))
      (goto-start-of-string)
      (setq p-stringstart (point))
      (backward-up-list 1) (forward-char 1) 
      (let ((form-name (string-to-symbol (scheme-name-under-point))))  ; only part before dash!
        (if (not (concatenate-formp form-name))
            (progn
               (goto-char p-stringstart)
               (nest concatenate-form)
               )))))
)


(defun concatenate-formp (x)
  (memq x (list concatenate-form)))



(defun word-interpretation ()
  "Return non-nil, if the context signals that the word under point is to be the selection.
Returns true if points happens to be located at the first character in a word"
 (save-excursion
  (backward-char 1) (or (looking-at CR) (looking-at " ") (looking-at "\""))))


; ---------------------------------------------------------------------------------------------------------------------------
; Variables that control embedding:
(defvar embed-require-match nil "Does the embed command require an exact match against html-laml-embed-tag-list" )
(defvar previous-embedding "" "The previously typed embedding name - for convenient repetition of embeddings.")
(defvar html-laml-embed-tag-list nil "A list of html and laml tag names (symbols) in which embedding a text string makes sense")

(defun embed ()
  "Embed the currenly selected string/word into a form prefixed by tag.
The embedding depends on the minor and major mode.
If smart-word-determination is true, a word under point is selected in case point is located at the word's first letter.
In addition, the variables embed-require-match, previous-embedding, html-laml-embed-tag-list, and 
nest-embed-attribute-insertion-map control the working of embed.
The parameter tag must be a symbol."
  (interactive)
  (cond ((and (is-provided 'scheme-elucidator-support) elucidator-mode (in-documentation-buffer-p))
           (let (
                 (embed-require-match nil)
                 (html-laml-embed-tag-list '(strong-prog-ref weak-prog-ref doc-ref source-marker))
                 (nest-embed-attribute-insertion-map '(
                                                       (weak-prog-ref file vers name)
                                                       (strong-prog-ref file vers name)
                                                       (doc-ref name)
                                                       (source-marker name)
                                                       )
                                                     )
                )
               (call-interactively 'internal-embed)))
        ((and (is-provided 'laml-chords-support) (eq major-mode 'chords-mode))
           (let (
                 (previous-embedding "")
                 (embed-require-match t)
                 (html-laml-embed-tag-list supported-chords)
                 (nest-embed-attribute-insertion-map nil)
                )
               (call-interactively 'internal-embed)))
        (t (call-interactively 'internal-embed))))

(defun internal-embed (tag)
  "An internal version of embed, supposed to be called via the embed function."
  (interactive 
    (list (string-to-symbol (completing-read "Embed in which Scheme function: " 
                               (trivial-alist (mapcar (function symbol-to-string) html-laml-embed-tag-list))
                               nil embed-require-match previous-embedding))))
  (let ((word-intr nil))
    (if (and smart-word-determination (word-interpretation))
        (progn (setq word-intr t) (mark-sexp 1)))    ; earlier: (mark-word 1)
    (if (looking-at "\"") (exchange-point-and-mark)) ; a trick to avoid problems with a embedding of the rear end of a string.
    (let ((p1 (min (point) (mark)))
          (p2 (max (point) (mark))))

      ; Old style stuff - not relevant for current LAML style.
      (if laml-single-string-approach (ensure-concatenation-context))

      ; Explicitly ensure that no punctuation is nested at the rear end of implicitly selected words:
      (if (and word-intr   
               (looking-at-punctuation-at (- p2 1)))
          (split-string-into-three p1 (- p2 1))
          (split-string-into-three p1 p2))

      (raw-nest tag)
      (remove-empty-siblings)
      (fix-pending-punctuation)
      (if (mark-marker) (move-marker (mark-marker) (point)))
      (setq previous-embedding (symbol-to-string tag))))
)

(defun is-provided (x)
  "Is the featur x provided"
  (memq x features))




; Do we see any punctuation at char position p?
(defun looking-at-punctuation-at (p)
  (save-excursion
    (goto-char p)
    (or (looking-at-char-number 46) (looking-at-char-number 44))))

; Is point located at character number n. n is an ASCII character number.
(defun looking-at-char-number (n)
 (let* ((str-under-point (buffer-substring-no-properties (point) (+ (point) 1)))
        (char-under-point (string-to-char str-under-point)))
   (= char-under-point n)))
    

(defun fix-pending-punctuation ()
 "A specialized function which fixes the problem of a punctuation character after an embedding."
 (save-excursion
  (forward-sexp 1)
  (while (blank-char-p (char-under-point))
                (forward-char 1))

  (if (looking-at "\"")
      (progn 
        (forward-char 1)
        (if (looking-at-punctuation-char (point))
            (progn
              (forward-char 1) 
              (if (looking-at-white-space (point))
                  (progn (split) (backward-sexp 1) (insert "_ "))
                  (progn (backward-char 2) (insert "_ ")))))))))


(defun looking-at-punctuation-char (p)
 (let ((ch (char-after p)))
  (or (= ch ?.) (= ch ?,) (= ch ??) (= ch ?!) (= ch ?:) (= ch 41)   ; 41 is end parenthesis
      (= ch 59))))                                                  ; 59 is semicolon

(defun looking-at-white-space (p)
 (let ((ch (char-after p)))
  (member ch (list 9 10 13 32))))

(defun looking-at-chars (p char-list)
  "is char at possition p member of char-list"
 (let ((ch (char-after p)))
  (member ch char-list)))  

(defun split ()
  "Split the string under point in two parts, and ensure (if laml-single-string-approach) that the strings parts are in a 
   concatenation context. Leave point in between the two string parts"
  (interactive)
  (if (looking-at "\"") (exchange-point-and-mark)) ; a trick to avoid problems with a embedding of the rear end of a string.
  (if laml-single-string-approach (ensure-concatenation-context))
  (split-string-into-two)
  (remove-empty-siblings)
)

; Assume as a precondition that we are inside a quoted string.
; Move point to the start quote of the string
(defun to-front-of-string ()
 (let ((cur-point (point)))
   (while (not (looking-at "\"")) (backward-char 1))))

(defun unsplit ()
  "Join neighbor verbatim strings together.
   You are supposed to select an inner character in one of the strings to be joined,
   or the start quote of one the strings.
   We assume that there are at least one space between verbatim strings. If not, this command will not work."
  (interactive)

  (save-excursion 

    (to-front-of-string)
    (forward-char -1)

    (while (and (string-to-the-left-p) (string-to-the-right-p))
           (forward-sexp -1) (forward-char -1))


    (save-excursion 
     (let* ((gaps (string-gaps (point)))
           (rev-gaps (reverse gaps)))

      (while (not (null rev-gaps))
        (progn
          (goto-char (car rev-gaps))
          (if (and (string-to-the-left-p) (string-to-the-right-p))
              (join-neighbor-strings))
          (setq rev-gaps (cdr rev-gaps))))))

    (backward-up-list 1)

    ; unnest if we see a con with a single string argument
    (if (and laml-single-string-approach (single-con-form-p (form-under-point)))
        (progn
          (unnest)
          (if (looking-at " ") (delete-char 1))
        )
    )

))
     


; Assume that possition p is at an initial quote in a string.
; Return a list of positions of gaps in between verbatim strings in front of us.
(defun string-gaps (p)
  (save-excursion (string-gaps-1 p)))

(defun string-gaps-1 (p)
  (forward-sexp 1)
  (if (and (string-to-the-left-p) (string-to-the-right-p))
      (cons (point) (string-gaps-1 (point)))
      nil))



; Is there a string to the left of point.

(defun string-to-the-left-p ()
  (save-excursion
    (while (blank-char-p (char-under-point))
      (forward-char -1))
    (equal QUOTE-CHAR (char-under-point))))


; Is there a string to the right of point.
(defun string-to-the-right-p ()
  (save-excursion
    (while (blank-char-p (char-under-point))
      (forward-char +1))
    (equal QUOTE-CHAR (char-under-point))))


;
(defun in-between-strings ()
 (save-excursion
  (cond ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point -1) QUOTE-CHAR))  ; at quote and also quote to the left
           t)
        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point 1) QUOTE-CHAR))   ; at quote and also quote to the left
           t)
        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point -1) SPACE-CHAR))
          (save-excursion
            (forward-char -1)
            (string-to-the-left-p)))
        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point 1) SPACE-CHAR))
          (save-excursion
            (forward-char 1)
            (string-to-the-right-p)))
        ((and (string-to-the-left-p) (string-to-the-right-p))
            t)
        (t nil))))



; assume we are in between to verbatim strings or at the leftmost string quote. Join them.
; Precondition: Point must be at white space in between strings, or one of the string quotes
(defun join-neighbor-strings ()

  (cond ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point -1) QUOTE-CHAR)) ; looking at " and a string quote to the left
           (delete-char 1) (delete-char -1)
           (if (not laml-single-string-approach)   ; always?
                (insert " "))
        )

        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point 1) QUOTE-CHAR)) ; looking at " and a string quote to the right
           (delete-char 2)
           (if (not laml-single-string-approach)   ; always?
                (insert " "))
        )

        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point -1) SPACE-CHAR)) ; looking at " and a space to the left of it
           (delete-char 1)  ; delete right quote
           (while (not (equal QUOTE-CHAR (char-under-point -1)))
                  (backward-delete-char-untabify 1))
           (backward-delete-char-untabify 1) ; delete left quote
           (if (not laml-single-string-approach)   ; always?
                (insert " "))
        )

        ((and (equal (char-under-point) QUOTE-CHAR) (equal (char-under-point 1) SPACE-CHAR)) ; looking at " and a space to the right of it
           (delete-char 1)  ; delete right quote
           (while (not (equal QUOTE-CHAR (char-under-point)))
                  (delete-char 1))
           (delete-char 1) ; delete left quote
           (if (not laml-single-string-approach)   ; always?
                (insert " "))
        )

        ((looking-at " ")
          (progn

            ; first join to the left:
            (if (not (equal (char-under-point) QUOTE-CHAR))
                (while (not (equal QUOTE-CHAR (char-under-point -1)))
                  (backward-delete-char-untabify 1)))
            (backward-delete-char-untabify 1) ; delete left string quote

            ; next join to the right:
            (while (not (equal QUOTE-CHAR (char-under-point 1)))
              (delete-char 1))
            (delete-char 2)             ; delete right quote string and char in front of it.

            (if (not laml-single-string-approach)
                (insert " "))))))



(defun single-con-form-p (x)
  (and (listp x)
       (= (length x) 2)
       (eq 'con (car x))
       (stringp (cadr x))))

; Return the lisp form under and before point. If not possible, return nil.
(defun form-under-point ()
 (save-excursion
   (let ((p (point)))
     (forward-sexp 1)
     (let ((q (point)))
       (if (and p q)
           (let ((form  (car (read-from-string (buffer-substring p q)))))
             form)
           nil)))))

(defun unembed ()
  "Unnest the currently selected form and join neighboring verbatim strings together.
   You are supposed to select the start parenthesis of the form which is to be unembedded."
  (interactive)

  (save-excursion  
    ;(if (not (looking-at "(")) (backward-up-list 1))
    (if (looking-at "(")
        (progn
          (unnest)
          ; now in between possible verbatim strings
      
      
          ; try to join verbatim strings to the left
          (if (in-between-strings) ; (and (string-to-the-left-p) (string-to-the-right-p))
              (join-neighbor-strings)
              (forward-char 1))
      
          ; go to end of possibly joined strings
          (while (not (equal QUOTE-CHAR (char-under-point)))
             (forward-char 1))
          (forward-char 1)
        
          ; try to join verbatim strings to the right
          (if (in-between-strings) ; (and (string-to-the-left-p) (string-to-the-right-p))
              (join-neighbor-strings)
              (forward-char -2))
      
          ; go to beginning of possibly joined strings
          (while (not (equal QUOTE-CHAR (char-under-point)))
             (forward-char -1))
          (forward-char -1)
      
          (backward-up-list 1)
      
          ; unnest if we see a con with a single string argument
          (if (and laml-single-string-approach (single-con-form-p (form-under-point)))
              (progn
                (unnest)
                (if (looking-at " ") (delete-char 1))
              )
          ))
        (message "To unembed select start parenthesis of form which you want to unembed"))))


; Given some position in a list, return the sibling constituents which are literal empty lists.
; Do not change the position of point.
(defun remove-empty-siblings ()
  (interactive)
  (let ((empty-string-reg-exp "\"\""))
   (save-excursion
    (backward-up-list 1) (forward-char 1)
    (while (not (looking-at ")"))
      (progn
        (if (looking-at empty-string-reg-exp)
            (progn
              (kill-sexp 1)
              (while (blank-char-p (char-under-point))
                (delete-char 1))
            )
            (forward-sexp 1))
        (forward-through-white-space))))))

(defun forward-through-white-space ()
  (skip-through (list 32 10 13)))  ; also include tab


; -----------------------------------------------------------------------------


;; Replace <, > and & with character entities that present these characters properly in HTML.
;; An interactive emacs lisp counterpart to the Scheme function html-protect.
(defun html-protect-buffer ()
  (interactive)
  (goto-char (point-min))
  (replace-string "&"  "&amp;")

  (goto-char (point-min))
  (replace-string "<" "&lt;")

  (goto-char (point-min))
  (replace-string ">" "&gt;")

)

; ---------------------------------------------------------------------------------------------------

(defun make-laml-schemedoc-manual (dir0 title kind)
  "Make a LAML SchemeDoc Manual script in dir0."
  (interactive "DMake a LAML manual script in which directory: 
sName of manual (without extension): 
nKind of manual: (1) SchemeDoc, (2) XML DTD, (3) Augmented XML DTD, or (4) custom manual: 
")
  (let ((dir (ensure-trailing-slash (expand-file-name dir0)))
       )
     (if (not (file-exists-p (concat dir title "." "laml")))
        (progn
            (let* ((manual-buf nil))

              ; Make the setup file
              (setq manual-buf
                    (make-a-file-from-laml-template 
                     (concat title "." "sdoc")
                     dir
                     (cond ((= kind 1) "manual-xml-in-laml-schemedoc")
                           ((= kind 2) "manual-xml-in-laml-xml-dtd")
                           ((= kind 3) "manual-xml-in-laml-xml-dtd-merge")
                           (t            "manual-xml-in-laml"))
                     'schemedoc-mode
                      (list
                      )
                     ))

              (show-buffer (selected-window) manual-buf)
              (set-buffer-file-coding-system (quote iso-latin-1))

              (message "Done.  Fill in details and then LAML process the buffer.")

              ))

        (progn
          (beep)
          (message "The manual already exists. Nothing done.")))))

(defun make-laml-schemedoc-manual-index (dir0 name)
  "Make a LAML Manual script in dir0."
  (interactive "DMake a LAML manual index script in which directory: 
sName of index (without extension): 
")
  (let ((dir (ensure-trailing-slash (expand-file-name dir0)))
       )
     (if (not (file-exists-p (concat dir name "." "laml")))
        (progn
            (let* ((manual-buf nil))

              ; Make the setup file
              (setq manual-buf
                    (make-a-file-from-laml-template 
                     (concat name "." "sdoc")
                     dir
                     "schemedoc-manual-index"
                     'schemedoc-mode
                      (list
                      )
                     ))

              (show-buffer (selected-window) manual-buf)
              (set-buffer-file-coding-system (quote iso-latin-1))

              (message "Done.  Fill in details and then LAML process the buffer.")

              ))

        (progn
          (beep)
          (message "The script already exists. Nothing done.")))))


(defun trim-string ()
  "Assume that the start quote of a string constant is selected. 
Remove initial and trailing spaces in the string"
  (interactive)
  (let ((p1 (point))
        (p2 (progn (forward-sexp 1) (backward-char 1) (point))))
    (backward-char 1)
    (while (or (looking-at " ") (looking-at "\n")) (delete-char 1) (backward-char 1))
    (goto-char p1) (forward-char 1)
    (while (or (looking-at " ") (looking-at "\n")) (delete-char 1))
    (goto-char p1)))

(defun fix-laml-form()
  "Assume that the parenthesis of a LAML form is selected. Fix it
with respect to trimming of strings, removal of superfluous, old-style con forms.
Fix subforms recursively."
  (interactive)
  (let ((p1 (point))
        (p2 (progn (forward-sexp 1) (backward-char 1) (point)))
        (done nil)
       )
   (backward-sexp 1)
   (while (not done)
     (cond ((or (looking-at "\",") (looking-at "\"\\.") )         ; string constant with initial comma or point
             (progn
              (forward-char 2) 
              (if (not (looking-at "\""))  ; not "." or ","
                  (progn (split) (backward-sexp 1) (insert "_ "))
                  (backward-char 2)) ))
           ((looking-at "\"") (trim-string))                    ; string constant
           ((looking-at "(con ") (progn (fix-laml-form) (unnest)))    ; old style (con forms
           ((and (looking-at "(") (not (looking-at "(a"))) (fix-laml-form))                        ; sub form - recursive processing
           
           (t 'nothing))
     (if (looking-at "\"\"") (kill-sexp 1))
     (if (not (near (point) p1)) (backward-sexp 1))
     (setq done (near (point) p1)))
   (backward-up-list 1)))

(defun near (p1 p2)
  (<= (abs (- p1 p2)) 1)) 


; ----------------------------------------------------------------------------------------------------------------------------------     
    
(defvar laml-open-form-tag-list '() "A list of possible form names for open-laml-form")
(defvar laml-open-require-match nil 
     "A boolean variable that controls if open-laml-form requires an exact match relative to laml-open-form-tag-list")
(defvar previous-new-laml-form "" "The previously typed new laml form  name - for convenient repetition of open-new-laml-form")


(defun open-laml-form ()
  "Open a new LAML form. Depends on the major and minor modes. Uses the variable nest-embed-attribute-insertion-map 
for determination of attributes of form-name, which are inserted automatically.
There is special ad hoc support of p.
Depends also on the variables laml-open-form-tag-list, laml-open-require-match, and nest-embed-attribute-insertion-map."
  (interactive)
  (cond ((and (is-provided 'scheme-elucidator-support) elucidator-mode (in-documentation-buffer-p))
           (let (
                 (laml-open-require-match nil)
                 (laml-open-form-tag-list '(strong-prog-ref weak-prog-ref doc-ref source-marker))
                 (nest-embed-attribute-insertion-map '(
                                                       (weak-prog-ref file vers name)
                                                       (strong-prog-ref file vers name)
                                                       (doc-ref name)
                                                       (source-marker name)
                                                       )
                                                     )
                )
               (call-interactively 'internal-open-laml-form)))
        ((and (is-provided 'laml-chords-support) (eq major-mode 'chords-mode))
           (let (
                 (previous-new-laml-form "")
                 (laml-open-require-match t)
                 (laml-open-form-tag-list supported-chords)
                 (nest-embed-attribute-insertion-map nil)
                )
               (call-interactively 'internal-open-laml-form)))
        (t (call-interactively 'internal-open-laml-form))))


(defun internal-open-laml-form (form-name)
  "An internal version of the function open-laml-form."
  (interactive 
    (list (string-to-symbol (completing-read "Open LAML form: " 
                               (trivial-alist (mapcar (function symbol-to-string) laml-open-form-tag-list))
                               nil laml-open-require-match previous-new-laml-form))))

  (if (is-inside-string)
      (split))

  (setq previous-new-laml-form (symbol-to-string form-name))
  (let ((indentation-level (measure-indentation-from-point))
        (p nil))
    (setq p (point))
    (insert "(")
    (insert (symbol-to-string form-name)) (insert " ")
    (cond ((eq form-name 'p)
             (progn (insert CR) 
                    (insert-indentation (+ 2 indentation-level)) 
                    (insert "\"\"")
                    (insert CR) 
                    (insert-indentation indentation-level)
                    (insert ")")
                    (backward-char 1) (backward-sexp 1) (forward-char 1)
                    ))
          (t (progn (insert-nesting-attributes form-name)
                    (backward-delete-char-untabify 1) (insert ")") (backward-char 1)))
    )
    (goto-char p)
   ))


; ---------------------------------------------------------------------------------------------------

(defvar activation-point 0 "The value of point for the last application of tidy-laml-form")
(defvar tidy-fill-column 80 "The initial value of the fill-column used in the command tidy-laml-form.")

(defun tidy-laml-form ()
  "Tidy up the LAML form under point. Breaks long strings relative to the value of the variable tidy-fill-column,
fixes LAML forms with fix-laml-form, and takes care of indentation. If tidy-laml-form is applied multiple
times at the same location, tidy-fill-column will be incremented for each applications.
When you are satisfied with the value of tidy-fill-column you should use tidy-laml-form-again instead
of tidy-laml-form. tidy-laml-form-again does not change tidy-fill-column."
  (interactive)

  ; Move point intelligently before tidying up:
  (if (is-inside-string)
      (progn
          (move-outside-string)
          (backward-up-list)))
  (if (not (looking-at "("))
      (backward-up-list))

  (if (= (point) activation-point)
      (setq tidy-fill-column (+ tidy-fill-column 10))
      (setq tidy-fill-column 80))
  (setq activation-point (point))

  (tidy-laml-form-internal))

(defun tidy-laml-form-again ()
  "Tidy up the LAML form under point. Uses the most recent value of tidy-fill-column, as assigned by 
the last activation of tidy-laml-form. Typically, you should use tidy-laml-form repeatedly 
at the same location to adjust tidy-fill-column to your needs. Following that you can use
tidy-laml-form-again for subsequent tidying up in your document to achieve a uniform text width."
  (interactive)
  ; Move point intelligently before tidying up:
  (if (is-inside-string)
      (progn
          (move-outside-string)
          (backward-up-list)))
  (if (not (looking-at "("))
      (backward-up-list))
  (setq activation-point (point))

  (tidy-laml-form-internal))
  


(defun tidy-laml-form-internal ()
  (setq activation-point (point))

  (if (not (looking-at "("))
      (error "tidy-laml-form: You should attempt to use tidy-laml-form directly at the form you wish to tidy up"))

  (fix-laml-form)
  (let ((indentation (measure-indentation-from-point))
        (constituents (number-of-constituents-of-form-under-point))
       )
    (down-list 1)
    (let ((form-length (measure-form-under-point))
          (x nil) ; the actual indentation
          (n 1)
          (first-time t)
         )
      (forward-sexp 1)  ; skipping form name
      (delete-white-space) 
      (insert " ")
      (setq x (distance-from-left-margin))

      (while (< n constituents)
        (let ((cur-form-length (measure-form-under-point)))
 
          (if (> (+ x cur-form-length) tidy-fill-column)
              (progn
                (if (not first-time) 
                    (progn (insert CR) (insert-indentation (+ indentation form-length 2))))       ; 2 = extra indentation
                (tidy-and-forward-constituent form-length indentation) 
                (delete-white-space)
                (setq x 0))
              (progn 
                (if (equal QUOTE-CHAR (char-under-point))
                    (tidy-string-constant)
                    (forward-sexp 1))
                (delete-white-space) 
                (insert " ")
              ))
           
          (setq n (+ n 1))
          (setq x (distance-from-left-margin))
          (setq first-time nil)
        )
      )
   )
   (backward-up-list 1)
   (fix-laml-form)  ; again
 )
)

(defun measure-indentation-from-point ()
 (let ((count 0))
  (save-excursion
    (backward-char)
    (while (looking-at " ") 
       (backward-char 1) 
       (setq count (+ count 1)))
    count)))

(defun insert-indentation (n)
  (insert (make-string n 32)))

(defun measure-form-under-point ()
  "Point is assume to point at the first char in a document constituent, such as start parenthesis or
   start string quote. Return the length of form."
  (save-excursion
    (let ((p0 (point)))
      (forward-sexp 1)
      (- (point) p0))))

(defun number-of-constituents-of-form-under-point()
 "Return the number of subforms in the form under point"
 (save-excursion
  (down-list 1)
  (let ((n 0))
    (while (forward-sexp-robust 1) 
       (setq n (+ n 1)))
    n)))
 

(defun tidy-and-forward-constituent (pre-indentation indentation)
  (cond ((equal QUOTE-CHAR (char-under-point))
           (tidy-string-constant) 
           (insert " ")
         )
        (t (forward-sexp 1))))

(defun tidy-string-constant ()
  (interactive)
  (if (= QUOTE-CHAR (char-under-point))
      (progn 
         (forward-char 1) ; first real char in string
         (let* ((ch (char-under-point))   ; a number
                (d (distance-from-left-margin)) 
                (init-indentation d)
               )
          (while (not (non-escaped-quote-p ch)) ; not equal end quote
            (progn
              (cond 
                ((and (or (= ch 32) (= ch 10) (= ch 9)) (> d tidy-fill-column))
                   (delete-white-space)
                   (insert-cr-indent init-indentation)
                   (backward-char 1)  
                )
                ((= ch 10)
                   (delete-white-space)
                   (insert " ") 
                   (backward-char 1)  
                )
                (t 'do-nothing) 
              )
              (forward-char 1)
              (setq ch (char-under-point))
              (setq d (distance-from-left-margin))
            )
          )
         )
         (forward-char 1)
      )))

(defun non-escaped-quote-p (ch)
 (save-excursion
   (let ((ch-before (progn (backward-char 1) (char-under-point))))
     (and (= QUOTE-CHAR ch) 
          (not (= BACKWARD-SLASH ch-before))))))
  

(defun insert-cr-indent (i)
  (insert CR)
  (insert-indentation i)) 

(defun delete-white-space ()
  (while (looking-at-white-space (point))
     (delete-char 1)))

(defun distance-from-left-margin ()
  "Return the number of chars on the current line up to point"
  (let ((n 0))
    (save-excursion
      (backward-char 1)
      (while (not (= (aref (char-under (point)) 0) 10))
         (progn 
           (setq n (+ n 1)) (backward-char 1)))
      n)))
   
(defun move-outside-string ()
  "Given as a precondition that the point is inside string, move to the beginning of it."
   (if (= QUOTE-CHAR (char-under-point)) ; the end string quote
       (backward-char 1))
   (while (not (non-escaped-quote-p (char-under-point)))
      (backward-char 1))) 

  
; ---------------------------------------------------------------------------------------------------
; Functionalty for calculating and inserting relative file paths.
; The main interactive function is insert-relative-file-path. 
; The main underlying function is relative-path-from.

(defun insert-relative-file-path (to-dir)
  "Insert the relative file path from the current directory, as determined by the expression (current-directory)
to to-dir. In interactive use, the user is asked to enter to-dir."
  (interactive "DPath to which directory or file: ")
  (let ((current-dir (current-directory))
        (to-dir-1 (eliminate-tilde-prefix to-dir)))
    (if current-dir
        (insert (relative-path-from current-dir to-dir-1))
        (error "Cannot determine current directory."))))

(defun eliminate-tilde-prefix (dir-path)
 "If dir-path is initiated with a tilde char ~/ prefix substitute it with the home directory of the current user.
This is only supposed to happen on Unix systems, in which case we rely on the environment HOME variable to get information about
the home dir string."
 (if (= ?~ (aref dir-path 0))
     (concat (user-home-dir) (substring dir-path 2 (length dir-path)))
     dir-path))

(defun user-home-dir ()
  "Return a forward slash terminated home directory."
  (let ((home (getenv "HOME")))
    (if (= ?/ (aref home (- (length home) 1)))   ; is home forward slash terminated
        home
        (concat home "/"))))


(defun relative-path-from (from-path to-path)
  "Return the relative file path from from-path to to-path. File paths are, in general,
assumed to be terminated by a forward slash. Handles both absolute and relative paths as input.
This function is NOT able to deal with tilde prefixes, suchs as ~/xxx/yyy."
 (let ((common-prefix (common-file-prefix from-path to-path)))
  (cond ((is-file-prefix-of from-path to-path)
            (file-path-difference to-path from-path))
        ((is-file-prefix-of to-path from-path)
            (let ((path-length (file-path-length (file-path-difference from-path to-path))))
               (replicate-string "../" path-length)))
        (common-prefix
           (let* ((suffix (file-path-difference from-path common-prefix))
                  (suffix-length (file-path-length suffix))
                 )
             (concat
                (replicate-string "../" suffix-length)
                (file-path-difference to-path common-prefix))))
        (t nil))))

(defun is-file-prefix-of (p1 p2)
  "Is the file path p1 a prefix of p2."
  (let ((p1-lgt (length p1))
        (p2-lgt (length p2)))
    (if (<= p1-lgt p2-lgt)
        (equal (substring p2 0 p1-lgt) p1)
        nil)))


(defun file-path-difference (p1 p2)
  "Return the file path difference between p1 and p2. The returned result is a suffix part of p1. 
As a precondition it is assumed that p1 is longer than p2."
 (let ((p1-lgt (length p1))
       (p2-lgt (length p2)))
  (substring p1 p2-lgt p1-lgt)))

(defun common-file-prefix (p1 p2)
  "Return the common file prefix of p1 and p2. If no such prefix exit, return nil."
  (let ((p1-list (file-path-to-path-list p1))
        (p2-list (file-path-to-path-list p2)))
    (common-file-prefix-1 p1-list p2-list)))

(defun common-file-prefix-1 (p1-list p2-list)
  "Return the common file prefixed of the listified paths p1-list and p2-list."
  (cond ((or (null p1-list) (null p2-list)) "")
        ((and (equal (car p1-list) (car p2-list)) (consp (car p1-list)) (eq (car (car p1-list)) 'winroot))
           (concat (cadr (car p1-list)) ":/" (common-file-prefix-1 (cdr p1-list) (cdr p2-list))))
        ((and (equal (car p1-list) (car p2-list)) (consp (car p1-list)) (eq (caar p1-list) 'unixroot))
           (concat "/" (common-file-prefix-1 (cdr p1-list) (cdr p2-list))))
        ((equal (car p1-list) (car p2-list))
           (concat (car p1-list) "/" (common-file-prefix-1 (cdr p1-list) (cdr p2-list))))

        (t "")))

(defun file-path-to-path-list (path) 
  "Convert the file path to a list of directory parts. if path is absolute, it starts with the list 
either (winroot drive-letter) or (unixroot). Path must contain at least the one part, and the trailing '/'"
  (cond ((absolute-windows-file-path-p path)
           (cons (list 'winroot (substring path 0 1)) (file-path-to-path-list-1 path 3 (length path))))
        ((absolute-unix-file-path-p path)
           (cons (list 'unixroot) (file-path-to-path-list-1 path 1 (length path))))
        (t (file-path-to-path-list-1 path 0 (length path)))))
           
(defun file-path-to-path-list-1 (path i j)
  (let ((separator-pos (find-in-string-from-position path ?/ i)))
    (if separator-pos
        (cons (substring path i separator-pos) (file-path-to-path-list-1 path (+ separator-pos 1) j))
        nil)))

(defun find-in-string-from-position (str char i)
  "Find the first index position of char in the string str. Start looking at position i in str.
If char is not part of str, return nil"
  (find-in-string-from-position-1 str char i (length str)))

(defun find-in-string-from-position-1 (str ch i j)
  (cond ((and (< i j) (eq (aref str i) ch)) i)
        ((and (< i j) (not (eq (aref str i) ch))) (find-in-string-from-position-1 str ch (+ i 1) j))
        (t nil)))
      
(defun file-path-length (p)
  "Return the number of directories involved in the file path p"
  (cond ((absolute-windows-file-path-p p)
           (count-specific-char-in-string p ?/ 3 (length p)))
        ((absolute-unix-file-path-p p)
            (count-specific-char-in-string p ?/ 1 (length p)))          
        (t (count-specific-char-in-string p ?/ 0 (length p)))))

(defun count-specific-char-in-string (str ch i j)
  "Count the number of ch chars in str from position i to position j"
  (cond ((>= i j) 0)
        ((= ch (aref str i)) (+ 1 (count-specific-char-in-string str ch (+ i 1) j)))
        (t (count-specific-char-in-string str ch (+ i 1) j))))

  
(defun absolute-windows-file-path-p (p)
  "Is the file path p an absolute file path in windows. Ala c:/xxx/"
  (and (>= (length p) 3) (= ?: (aref p 1)) (= ?/ (aref p 2))))

(defun absolute-unix-file-path-p (p)
  "Is the file path p an absolute file path"
  (= ?/ (aref p 0)))

(defun absolute-file-path-p (p)
  "Is the file path p an absolute file path"
  (or (absolute-windows-file-path-p p) (absolute-unix-file-path-p p)))
  

(defun replicate-string (str n)
  "Replicate str n times and return the resulting string. In case n is zero or negative, return the empty string. OK"
  (cond ((<= n 0) "")
        (t (concat str (replicate-string str (- n 1))))))

; ---------------------------------------------------------------------------------------------------------------

(defun one-line-form ()
  "Make the current form a one-line form. The form is selected by pointing at the initial parenthesis of the form"
  (interactive)
  (let ((p0 (point))
        (p1 (progn (forward-sexp 1) (point))))
    (if (> p1 (+ p0 2)) (backward-char 2))
        (while (> (point) p0)
          (cond ((and (looking-at-white-space (point)) (= (char-under-point -1) 41))  ; just before ending paren
                    (progn (delete-char 1) (backward-char 1)))
                ((and (looking-at-white-space (point)) (not (looking-at-white-space (- (point) 1))))
                    (progn (delete-char 1) (insert " ") (backward-char 2)))
                ((looking-at-white-space (point))
                    (delete-char 1) (backward-char 1))
                (t  (backward-char 1))))))