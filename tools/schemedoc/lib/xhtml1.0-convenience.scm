;=>man/xhtml10-convenience.sdoc

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2002  Kurt Normark, normark@cs.auc.dk.
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

;;;; This library contains a number of convenient functions on top of the XHTML mirrors.
;;;; The functions in this library are intended to be common for XHTML mirrors in LAML.
;;;; Notice, however, that some of the functions will only work with HTML1.0 transitional.<p>
;;;;
;;;; The collection in this library can be regarded as personal convenience stuff. You can use it
;;;; if you find it useful - or you can develop your own convenience stuff on top of the XHTML mirror library.<p>
;;;;
;;;; The collection also serves as backward compatibility functions in relation to older LAML software. Many
;;;; functions in this library originally served as ad hoc Scheme markup functions. As of this library, the
;;;; functions have been reimplemented on top of the XHTML libraries.
;;;; .title Reference Manual of the XHTML convenience library


;;; Basic HTML extension stuff. 
;;; In this section we implement mirrors of HTML stuff such as comment and character entities.
;;; Also there are a number of convenient white space functions (horizontal and vertical).
;;; As a special, but very useful function, we include an html-protect function which provides for presentation
;;; of verbatim HTML documents in a browser. There is also some basic javascript support in this section.

; Backward compatibility only:
(define con list)
; (define con-space list)

; The HTML4.01 loose document type clause. Just a backward compatibility alias.
(define doctype-clause document-type-declaration)

;; If x is a number return a numbered character entity.
;; If x is a symbol og string, return a named character entity.
(define (character-entity x)
 (char-ref x))

;; The copyright character entity
(define copyright (character-entity "copy"))


;; Return n space special characters (horizontal space)
(define (space n)
  (make-list n (character-entity "nbsp")))

;; Return n space special characters 
(define horizontal-space space)

;; Return n vertical spaces, i.e., n instances of the p tag.
(define (vertical-space n)
  (if (= n 0) '()
      (cons (space 1) (cons (p) (vertical-space (- n 1))))))

;; Protect HTML tags such that an HTML document can be shown verbatim in a browser.
;; Transliterate angle brackets in str to the particular html character entities.
;; In normal XML-in-LAML you should not use this function.  Only use in the rare cases where HTML markup is handled as text.
;; .misc The intended functionality of html-protect is integrated in every XML-in-LAML mirror function due to the use of the standard HTML transformation table.
(define (html-protect str)
 (transliterate
   (transliterate
     (transliterate 
         str #\& "&amp;")  
     #\> "&gt;")
    #\< "&lt;"))

;; Translate the Danish letters in str to a string with appropriate use of HTML character entities.
(define (in-danish str)
 (letrec ((in-danish-1 
            (lambda (str letter-numbers)
              (cond ((null? letter-numbers) str)
		    (else (in-danish-1
			   (transliterate str (as-char (car letter-numbers)) (character-entity (car letter-numbers)))
			   (cdr letter-numbers)))))))
  (let ((danish-letter-numbers (list 230 248 229 198 216 197)))
    (in-danish-1 str danish-letter-numbers))))


;; Return a Javascript calling form, given function function-name and parameters.
;; Returns a string of the form: function-name(parameters).
;; This function adds commas between the actual Javascript parameters.
(define (js-call function-name parameters)
  (string-append function-name 
                   "(" 
                      (string-merge (map as-string parameters)
                                    (make-list (- (length parameters) 1) ", "))
                   ")"))



;; Return a manifest javascript array given its elements
(define (js-string-array elements)
  (string-append "[" 
                 (string-merge (map string-it-single (map as-string elements))
                               (make-list (- (length elements) 1) ","))
                 "]")
)



; ---------------------------------------------------------------------------------------------------
;;; Convenience variants of the HTML mirror functions. 
;;; The functions in this section are either quite close to the HTML mirror functions or very simple.

;; Return an a tag constructructed from the URL and the anchor. 
;; If no anchor is provided uses the url as anchor text.
(define (a-tag url . optional-parameter-list)
 (let ((anchor (optional-parameter 1 optional-parameter-list url)))
  (a anchor 'href (as-string url))))

;; A variant of a tag which supports a target attribute of the a-tag (where in browser to show the result).
(define (a-tag-target url anchor target)
  (a
    anchor
    'href (as-string url)
    'target (as-string target)))

;; Name the current place by means of an a tag with name attribute
(define (a-name name)
  (a ""
    'name (as-string name)))

;; A 'self referential' use of the a (anchor) element. Supply the 'href attribute, which also becomes the textual contents of the a element.
(define a-self-ref 
 (xml-in-laml-abstraction 
  (lambda (cont attr)
    (a attr (defaulted-get-prop 'href attr "???")))))

;; Return a mail link by means of the mailto facility in an a tag.
;; .form (mail-link email-adr [anchor-name])
(define (mail-link email-adr . optional-parameter-list)
 (let ((anchor-name (optional-parameter 1 optional-parameter-list email-adr)))
  (a-tag (string-append "mailto:" email-adr) anchor-name)))


;; Returns h tags, h1 if i=1, h2 if i=2, etc.
;; If i is higher than 6, use size 6.
(define (h i x)
  (cond ((= i 1) (h1 x))
        ((= i 2) (h2 x))
        ((= i 3) (h3 x))
        ((= i 4) (h4 x))
        ((= i 5) (h5 x))
        ((>= i 6) (h6 x))
  ))


;; Returns a font tag with size and color attributes.
;; .parameter size A number between 1 and 6, or the symbol normal (giving size 3).
;; .parameter color A color as accepted by the function rgb-color-encoding.
;; .reference "applied function" "rgb-color-encoding" "color.html#rgb-color-encoding"
(define (font-1 size color x)
  (font  x 'size (convert-size size) 'color (rgb-color-encoding color)))

(define (convert-size size)
  (if (and (symbol? size) (eq? size 'normal)) "3" (as-string size)))

;; Like font, but only supports size.
;; size is a number and color is a rgb list of three, decimal integers or
;; a color symbol.
(define (font-size size x)
  (font  x 'size (convert-size size)))

;; Like font, but only supports color.
;; color is a rgb list of three, decimal integers
(define (font-color color x)
  (font  x 'color (rgb-color-encoding color)))


;; Generate a function which appends element. 
;; As an example use, (html-appender (p)) is a function of one parameter, which appends a p tag to the argument of the function.
(define (html-appender element)
  (lambda (existing-stuff)
    (con existing-stuff explicit-space element)))

;; Return a html fonted version of str. Initial letter is sized base-size+1.
;; The rest is size base-size.
;; Returns a list, not an ast. If rendered directly, use render-1.
(define (font-rise str base-size)
  (con
     (font-size (+ base-size 1) (substring  str 0 1)) explicit-space
     (font-size base-size (substring  str 1 (string-length str)))))

;;; Lists and trees.
;;; In this section there are various convenient functions which renders lists and trees.


;; Return a flat list, separate by breaks. 
;; The parameter lst is a list of items. 
;; In this version, returns a list, which cannot be rendered directly. 
;; However, render-1 works on the result.
(define (br-list lst)
  (map (lambda(el) (con el (br))) lst))

;; A convenient alias for br-list
;; .form (brl lst)
;; .internal-references "see also" "br-list"
(define brl br-list)


;; Make a definition list.
;; lst is a list of lists. Each inner list must be of length two,
;; dt and dd respectively. I.e. definition terms and the defintion proper, resp.
;; As a special case supported, the inner list can be of lenght one, in which case
;; the dd is considered empty.
(define (definition-list lst)
  (dl
   (map (lambda(el) 
	  (let ((dt-data (car el))
		(dd-data (if (= 1 (length el)) "" (cadr el))))
	    (con (dt  dt-data)
		 (if (equal? dd "")
		     ""
		   (dd dd-data)))))
	lst)))


;; Show tree as an indented, bulleted list.
;; A tree is a list.
;; The first element of the list is the root.
;; The tail of the list is the list of subtrees.
;; A subtree, which is a leaf, can be given as a string (cdata) or a contents element.
;; <pre>
;; Example   (a (b c d) (e (f g) h))
;;
;;               a
;;             /   \\
;;            b     e
;;           / \\   / \\
;;          c  d  f   h
;;                |
;;                g
;; </pre>
(define (ul-tree tree)
  (cond ((or (cdata? tree) (ast? tree)) (ul (li 'type "disc" tree)))
        ((pair? tree) 
	 (ul
  	    (li (car tree) 'type "disc"
  	      (map ul-tree (cdr tree)))))))

;; A spacy variant of the HTML li element.
(define lis
  (xml-in-laml-abstraction 
    (lambda (cont attr)
      (li cont attr 'css:margin-bottom "3mm"))))


; --------------------------------------------------------------------------------------------------------
;;; Table functions.
;;; In this section there is a number of table functions which
;;; maps a list of rows (list of lists) to an HTML table. 
;;; Older LAML software depends on these. I do not recommend use of these functions in new software. 
;;; Instead, use the HTML table mirror function directly.

;; Return a table with elements from list-of-list.
;; The sublists of list represent the rows in the table. The border is an optional parameter.
;; Corresponds to the old function table in the ad hoc html library. 
;; .parameter list-of-list the list of table row lists.
(define (table-0 list-of-list . optional-parameter-list)
  (let ((table-row 
         (lambda (lst) (tr
			(map (lambda (cell)
			       (td cell))
			     lst))))
	(border (optional-parameter 1 optional-parameter-list "1")))
    (table
     (con 
      (tbody
       (map table-row list-of-list)))
     'border (as-string border))))


;; A more versatile variant of table-0.
;; A variant of table-0 which requires border (an integer, 0 if no border),
;; a list of cell widths, a list of column colors, the table contents (list-of-list - list of rows), and an optional valign parameter.
;; The valign parameter corresponds to the HTML valign attribute, the possible value of which are
;; "top", "middle", "bottom", and "baseline" (a string).
(define (table-1 border cell-width-list cell-color-list-1 list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row)
	  (tr
	   (map (lambda (cell width color-1)
		  (td 
		  cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-color-encoding color-1)
		   )
		  )
		row cell-width-list cell-color-list-1))
	  )
	list-of-list
	)
       ))
     'border (as-string border))))


;; A variant of table and table-1 which supports a header row.
(define (table-2 border cell-width-list cell-color-list-1 header-list list-of-list)
  (table
   (con
    (tbody
     (cons   ; if con then a strange error - research it and try to catch it in AST composition phase
      (tr
       (map (lambda (h)(th h)) header-list))
      (map
       (lambda (row)
	 (tr
	  (map (lambda (cell width color-1)
		 (td 
		  cell
		  'width (as-string width) 'valign "top" 'bgcolor (rgb-color-encoding color-1)
		  )
		 )
	       row cell-width-list cell-color-list-1)
	  ))
       list-of-list
       ))))
  'border (as-string border)))

;; A transparant variant of table-1 - without specification of a column color list.
;; The cell color becomes identical with the background.
(define (table-3 border cell-width-list  list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row)
	  (tr
	   (map (lambda (cell width)
		  (td 
		   cell
		   'width (as-string width) 'valign va
		   )
		  )
		row cell-width-list))
	  )
	list-of-list
	)
       ))
     'border (as-string border))))

;; A variant of table-1 with a row color list instead of a column color list.
;; The length of row-color-list must be the number of rows in the table.
(define (table-4 border cell-width-list row-color-list list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row row-color)
	  (tr
	   (map (lambda (cell width)
		  (td 
		   cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-color-encoding row-color)
		   )
		  )
		row cell-width-list)))
	list-of-list row-color-list
	)
       ))
     'border (as-string border))))

;; A variant of table-4 that allows individual background coloring of table cells.
;; The parameter list-of-color-list is a list of row colors, whereas row-color-list in table-4
;; is a fixed list of colors that apply to a row. Thus, the structure of list-of-color-list
;; is identical to the structure of list-of-list.
;; .internal-references "similar function" "table-4"
(define (table-5 border cell-width-list list-of-color-list list-of-list . optional-parameter-list)
  (let ((va (as-string (optional-parameter 1 optional-parameter-list "top"))))
    (table
     (con
      (tbody
       (map
	(lambda (row row-color-list)
	  (tr
	   (map (lambda (cell width row-color)
		  (td 
		   cell
		   'width (as-string width) 'valign va 'bgcolor (rgb-color-encoding row-color)
		   )
		  )
		row cell-width-list row-color-list)
	   ))
	list-of-list list-of-color-list
	)
       ))
     'border (as-string border))))

;; Return a banner (represented as a table) with left, middle, and right justified contributions.
;; The optional parameter distribution-percentages gives the relative sizes 
;; of the left, middle and right fields.
;; .form (left-middel-right-banner left middle right [distribution-percentages])
;; .parameter left The left portion of the banner.
;; .parameter middle The middel portion of the banner.
;; .parameter right The right portion of the banner.
;; .parameter distribution-percentages A list of three integers (percent numbers) the sum of which is expected to be 100. Defaults to '(33 34 33).
(define (left-middle-right-banner left middle right . optional-parameter-list)
 (let* ((distribution-percentages (optional-parameter 1 optional-parameter-list '(33 34 33)))
        (left-percent (string-append (as-string (first distribution-percentages)) "%"))
        (middle-percent (string-append (as-string (second distribution-percentages)) "%"))
        (right-percent (string-append (as-string (third distribution-percentages)) "%"))
       )
  (table
   (con
   (tbody
    (con
    (tr
     (con
      (td
       (con (font-size 2 left))
       'width left-percent 'align "left" 'valign "top")

      (td
       (con (font-size 2 middle))
       'width middle-percent 'align "center" 'valign "top")

      (td
       (con (font-size 2 right))
       'width right-percent 'align "right" 'valign "top")
      )
     ))))
   'border "0px" 'cellpadding "0" 'cellspacing "0" 'width "100%")))


;; Return a banner (represented as a table) with left and right justified contributions.
;; The optional parameter distribution-percentages gives the relative sizes 
;; of the left and right fields.
;; .form (left-right-banner left right [distribution-percentages])
;; .parameter left The left portion of the banner.
;; .parameter right The right portion of the banner.
;; .parameter distribution-percentages A list of three integers (percent numbers) the sum of which is expected to be 100. Defaults to '(50 50).
(define (left-right-banner left right . optional-parameter-list)
 (let* ((distribution-percentages (optional-parameter 1 optional-parameter-list '(50 50)))
        (left-percent (string-append (as-string (first distribution-percentages)) "%"))
        (right-percent (string-append (as-string (second distribution-percentages)) "%"))
       )
  (let ((font-size (lambda (x y) y))) ; local redef to circumvent improper use of font-size
    (table
     (con
      (tbody
       (con
        (tr
         (con
          (td
           (con (font-size 2 left))
           'width left-percent 'align "left" 'valign "top")

          (td
           (con (font-size 2 right))
           'width right-percent 'align "right" 'valign "top")
          )
         ))))
     'border "0" 'cellpadding "0" 'cellspacing "0" 'width "100%")))) 



;; Return the standard LAML top banner with time of generation, copyright, and home icon
(define (laml-top-banner)
 (let ((yr (car (time-decode (current-time)))))
  (left-middle-right-banner
     (when-generated)
     (span "Copyright" copyright  (as-string yr) _ ","  "Kurt Nørmark")
     (laml-home-button 0 "laml-home.gif"))))

;; Makes a horizontal menu in terms of a table with links.
;; The table is made on the basis of parameter mini-menu-list, which is a list of
;; menu entries. A menu entry is a list of anchor-text and URL pairs (lists of
;; two strings). Dark-color must be some dark color.
(define (mini-menu mini-menu-list dark-color)
 (letrec ((mini-menu-entry (lambda (e) 
                             (let ((text (car e))
                                   (url (cadr e)))
                              (con (a (font-1 2 white text) 'href url 'css:text-decoration "none")
                                   ))))
          (lgt (length mini-menu-list)))
   (table-1
     1
     (make-list lgt 160)
     (make-list lgt dark-color)
     (list (map mini-menu-entry mini-menu-list)))))


; ---------------------------------------------------------------------------------------------------
;;; HTML input form functions. 
;;; A number of convenient functions which supports the work with HTML input forms.

;; Embed x in to form of kind POST, which activates url upon form completion. 
;; Corresponds to the old function form in the ad hoc html library. 
(define (form-1 cgi-url x)
  (form x 'method "post" 'action cgi-url))

;; Embed x into a multipart form. Activate cgi-url when the form is submitted.
;; A multipart form is used for file uploading. Files are written into 
;; target-directory when uploaded.
;; The parameter target-directory-url gives the URL of the directory, in which the file is uploaded.
;; This is used for subsequent WWW retrival of the file.
;; .reference "accompanying function" "multipart-decode" "encode-decode.html#multipart-decode"
(define (multipart-form cgi-url target-directory target-directory-url x)
  (form 
     (con
       (hidden-line "target-directory!!!" target-directory)
       (hidden-line "target-directory-url!!!" target-directory-url)
       x
      )
     'method "post" 'enctype "multipart/form-data" 'action cgi-url))

;; Return an input tag of type checkbox. The name is a string or symbol which identifies the checkbox.
;; Checked is an optional boolean parameter. If checked is #t, the checkbox will be checked initially.
;; Returns the string true to the form processing application if checked.
(define (checkbox name . checked)
 (let ((checked1 (if (null? checked) #f (car checked))))
  (if checked1 
    (input 'type "checkbox" 'checked "checked" 'value "true" 'name (as-string name))
    (input 'type "checkbox" 'value "true" 'name (as-string name)))))

;; Return an input tag of type radio. 
;; checked is a boolean parameter, i.e. true or false (in Scheme sense). 
(define (radio-button value group-name . checked)
 (let ((is-checked (and (not (null? checked)) (boolean? (car checked)) (car checked))))
  (if is-checked
    (input 'type "radio" 'checked "checked" 'value (as-string value) 'name (as-string group-name))
    (input 'type "radio" 'value (as-string value) 'name (as-string group-name))
  )))

;; Return an input tag of type text. 
;; The name is a string of symbol which identifies the text line.
;; Size is the text line width in character positions.
;; Value is the initial value on the text line.
(define (text-line name size value)
  (input 'type "text" 'name (as-string name) 'size (as-string size) 'value (as-string value)))

;; Return an input tag of type hidden. 
;; The name is a string or symbol which identifies the hidden line.
;; Value is the string contents of the hidden line.
(define (hidden-line name value)
  (input 'type "hidden" 'name (as-string name) 'value (as-string value)))

;; Return an input tag of type file.
;; Such an input tag is used for file uploading.
;; The name of the uploading is name.
(define (file-upload name)  
  (input 'type "file" 'name (as-string name)))

;; Return an input tag of type password. 
;; The name is a string of symbol which identifies the password.
;; Size is the line width in character positions.
;; Value is the initial contents of the password field (not very useful...).
(define (password-line name size value)
  (input 'type "password" 'name (as-string name) 'size (as-string size) 'value (as-string value)))

;; Return an input tag of type submit. Renders a button. 
;; Value is the string label of the button. If the optional parameter name
;; is given it identifies a particular submit button with a name, value pair in the submitted data.
;; .form (submit value [name])
(define (submit value . optional-parameters)
 (let ((name (optional-parameter 1 optional-parameters #f)))
  (if name
      (input 'type "submit" 'value (as-string value) 'name (as-string name))
      (input 'type "submit" 'value (as-string value)))))

;; Return an input tag of type reset. 
;; Value is the string label of the button.
(define (reset value)
  (input 'type "reset" 'value (as-string value)))

;; Return a select tag, defining  multiple choice menu. Name is a string or symbol which identifies the selection.
;; Value-list is a list of the values to be returned upon selection.
;; Contents-list is the list of contents to be shown in the menu.
;; Selected-value is an optional value, which is to be selected initially. This value should be a member of value-list.
;; Corresponds to the old function select in the ad hoc html library. 
(define (select-1 name value-list contents-list . selected-value)
 (let* ((selected (if (null? selected-value) "" (car selected-value)))
        (body (map (lambda (value contents)
                           (if (equal? selected value)
                               (option (as-string contents) 'value (as-string value) 'selected "selected") 
                               (option (as-string contents) 'value (as-string value)))) 
			 value-list contents-list))
       )
   (select body 'name (as-string name))))

;; Return a textarea form. 
;; Rows is the number of rows of the text area.
;; Cols is the number of columns measured in characters.
;; Contents is the initial contents of the text area.
;; Corresponds to the old function textarea in the ad hoc html library. 
(define (textarea-1 name rows cols contents)
  (textarea (as-string contents) 'name (as-string name) 'rows (as-string rows) 'cols (as-string cols)))


; ---------------------------------------------------------------------------------------------------
;;; Multi column lists. 
;;; The functions in this section return multi-column lists. Given a list of elements the functions
;;; return a table in which the elements have been arranged in a number of columns. The first function,
;;; multi-column-list, arranges the elements in row major order. The two last functions arrange the
;;; the elements in column major order. These are the most advanced functions due to the way tables
;;; are organized in HTML.


;; Return a multi-column list, row major, with columns columns.
;; Columns (the first parameter) must be at least 2.
;; The total width (sum of column widths) is given as the last parameter.
;; Internally, a HTML table with zero border is formed and returned.
(define (multi-column-list columns elements total-width)
 (let* ((lgt (length elements))
        (rem (remainder lgt columns))
        (elements-2 (cond ((= lgt 0) (make-list columns " "))     ; ensure that list length is a multiplum of column, and at least column
                          ((= 0 rem) elements)
                          (else (append elements (make-list (- columns rem) " ")))))
        (rows (sublist-by-rows columns elements-2))
        (column-width (quotient total-width columns))
        (column-widths (make-list columns column-width)))
    (table-3 0 column-widths rows)))


;; Return a two column list, column major.
;; total-width (sum of column widths) is the width you want the resulting table to have.
;; Internally, a HTML table with zero border is formed and returned.
(define (two-column-list elements total-width)
 (let* ((lgt (length elements))
        (rem (remainder lgt 2))
 
        ; not used any more
        ; ensure that list length is a multiplum of column, and at least column
        (elements-2 (cond ((= lgt 0) (make-list 2 " ")) 
                          ((= 0 rem) elements)
                          (else (append elements (make-list (- 2 rem) " ")))))

        (rows (sublist-by-2columns elements " "))
        (column-width (quotient total-width 2))
        (column-widths (make-list 2 column-width)))
    (table-3 0 column-widths rows)))

;; Return an n column list, column-major, of the element list (second parameter).
;; This is a generalized version of two-column-list.
;; total-width (sum of column widths) is the width you want the resulting table to have.
;; n is the number of columns.
;; Internally, a HTML table with zero border is formed and returned.
(define (n-column-list n elements total-width)
 (let* ((lgt (length elements))
        (rows (sublist-by-columns n elements " "))
        (column-width (quotient total-width n))
        (column-widths (make-list n column-width)))
    (table-3 0 column-widths rows)))


;;; Images and image file access. 
;;; The functions in this section determine how images are accessed from this
;;; and other libraries.

;; The URL where the author of this library keeps a number of images (icons).
;; This variable is used if image-file-access is the symbol net.
(define kn-internet-image-path "http://www.cs.auc.dk/~normark/images/")

;; Determination of the actual file path to images in html files.
;; This function depends on the variable image-file-access, which MUST be defined external to this library.
;; The value of image-file-access can be changed via the procedure set-image-file-access!
;; One of the following symbols apply: local, parent, net, sub-directory, or fixed. Default is local.
;; local means that images are taken from the current directory.
;; parent means that images are tagen from ../images.
;; sub-directory means that images are taken from ./images.
;; fixed means that images are taken from fixed-image-directory (a variable which must be defined
;; external to this library).
;; Finally, net means that images are taken from kn-internet-image-path (a variable).
(define (image-file-path)
  (cond ((eq? image-file-access 'local) "")
        ((eq? image-file-access 'parent) "../images/")
        ((eq? image-file-access 'sub-directory) "./images/")
        ((eq? image-file-access 'net) kn-internet-image-path)
        ((eq? image-file-access 'fixed) fixed-image-directory)
  ))

;; Set the image-file-access variable. 
;; mode is symbol, either local, parent, sub-directory, net, or fixed.
;; Relative to the hmtl directory, where the target files are written.
(define (set-image-file-path! mode)
  (set! image-file-access mode))

;; Return the full path/address of the image file named file-name. 
;; Relies of the setting of the variable image-file-access via the procedure set-image-file-path!
;; File name must include file extension
(define (image-file file-name)
  (string-append (image-file-path) file-name ))


;; Return an img tag, in which a file on file-name is presented. An optional width parameter is supported. 
;; Corresponds to the old function img in the ad hoc html library. 
(define (img-0 file-name . width)
 (if (not (null? width))
  (img 'alt "" 'src (as-string file-name) 'width (as-string (car width)) 'border "0")
  (img 'alt "" 'src (as-string file-name) 'border "0")))

;; A variant of img which presents an image with a border
(define (img-with-border file-name . width)
 (if (not (null? width))
  (img 'src (as-string file-name) 'width (as-string (car width)))
  (img 'src (as-string file-name))))


;; Return an HTML a tag (anchor) which links to the LAML software home page via an small gif icon.
;; If possible, a relative URL is used as the href attribute.
;; The parameter extra-level is an extra level to add. Normally, extra-level is 0 (zero).
;; As an example, extra-level should be 1 in case HTML files are organized
;; in a sub-directory relative to the laml source file.
;; Text-or-image is either the symbol 'text or 'image, or a string. If 'text, a textual anchor is used.
;; if 'image, a small 'laml house' is used as image. If text-or-image is a string it is a name of an image file
;; in the laml image directory (including file name extension, excluding any file path).
;; The optional start-dir gives the directory, in which the home button is to be placed; It defaults to (startup-directory).
(define (laml-home-button extra-level text-or-image . start-dir)
 (let* ((start-dir-1 (if (null? start-dir) (startup-directory) (car start-dir)))
        (url-of-laml (laml-home-url-prefix extra-level start-dir-1))
        (help-text 
           (if (equal? url-of-laml laml-absolute-url-prefix)
               "The LAML software home page at Aalborg University"
               "The local LAML software home page"))
        (image-file 
           (cond ((eq? text-or-image 'text) "")      ; not defined
                 ((eq? text-or-image 'image) "images/blue-house.gif")
                 ((string? text-or-image) (string-append "images/" text-or-image))
                 (else "???")))
       )
   (a
     (cond ((eq? text-or-image 'text) "LAML home")
           ((or (eq? text-or-image 'image) (string? text-or-image))
                    (img 'src (string-append url-of-laml image-file) 'alt help-text 'border "0"))
           (else "LAML home"))
     'href (string-append url-of-laml "index.html")
     'title help-text
     'target "_top")))


; ---------------------------------------------------------------------------------------------------
;;; Indenting, boxing, and framing.
;;; Here is a number of functions of indentation and frame making.

;; Indent text with p pixels

(define indent-pixels
  (xml-in-laml-positional-abstraction 1 0
    (lambda (p c a)
       (div 'css:margin-left (string-append (as-string p) "px") a c))))

; (define (indent-pixels p text)
;   (div 'css:margin-left (string-append (as-string p) "px") text))

;; Show text in a column, narrowed from both left and right with p pixels
(define (narrow-with-pixels p text)
  (table-3 0 (list p "*" p) 
    (list (list "" text ""))))

;; Shown text in a simple frame. 
;; Has nothing to do with HTML frames.
;; Corresponds to the old function frame in the ad hoc html library. 
(define (frame-1 text) 
  (table-3 1 (list "*") 
    (list (list text))))

;; Embed text in an invisible (border-less) table with one cell.
;; The optional parameter, width, can be used to control the width of the table cell (defaults to "*").
;; .form (box text [width])
(define (box text . optional-parameter-list) 
 (let ((width (optional-parameter 1 optional-parameter-list "*")))
  (table-3 0 (list width)
    (list (list text)))))

;; Present the contents-list, which is a list of elements, in a narrow
;; column of width, separated with activations of separator-fn.
(define (narrow separator-fn width . contents-list)
  (let ((separator-list (make-list (- (length contents-list) 1) (separator-fn))))
   (table-3 0 (list width) 
    (list 
      (list (merge-lists-simple contents-list separator-list))))))

;; Embed text into a color frame. It is the background which is colored.
(define (color-frame text color) 
  (table-1 0 (list "*") (make-list 1 color)
    (list (list text)) "bottom"))

;; As color-frame, but this function supports and extra widht parameter. This is an integer: the with of the frame in pixels.
(define (color-frame-width text color width) 
  (table-1 0 (list width) (make-list 1 color)
    (list (list text)) "bottom"))

;; Like frame, but with an extra width parameter. This is an integer: the with of the frame in pixels.
(define (frame-width text width) 
  (table-3 1 (list width) ; (make-list 1 slide-background-color)
    (list (list text))))

;; Embed text into a centered frame
(define (center-frame indentation text)
  (center
   (narrow-with-pixels indentation
     (frame-1 text))))


;;; Alphabetical index arrays. 
;;; The alphabetic index arrays are useful for presentation of alphabets linking to separate pages in a large index.

;; Return an 'array' of letter links to #letter
(define (alphabetic-link-array)
 (map 
  (lambda (letter)  (con (a-tag (string-append "#" letter) (capitalize-string-nd letter))  (horizontal-space 1)))
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" 
	"p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "æ" "ø" "å")))

;; Return an 'array' of letter links to (string-append target-file-prefix "-" letter ".html") for all letters in alphabet. 
;; This is a generalized version of alphabetic-link-array.
;; target-file-prefix is a prefix of the file names, in which the index files are located.
;; alphabet is a list of letters, for which to generate index links from the alphabet arrays. Some letters
;; may be missing from the alphabet compared with a complete alphabet.
;; emphasis-letter is an optional letter which we emphasize in the link array
(define (alphabetic-link-array-1 target-file-prefix alphabet . emphasis-letter)
  (let* ((em-let (if (not (null? emphasis-letter)) (as-string (car emphasis-letter)) #f))
         (alphabet-1 (map as-string alphabet)))
    (map 
     (lambda (letter) 
       (con
	(a-tag (string-append target-file-prefix "-" letter ".html")
	       (if (and em-let (equal? em-let letter))
		   (font-1 4 red (b (capitalize-string-nd letter)))
		   (capitalize-string-nd letter)))
	" "
       ))
     alphabet-1)))

; ---------------------------------------------------------------------------------------------------
;;; Substring Coloring.
;;; The function colorize-substring in this section is able to colorize specified substrings
;;; of a given string.

;; This is an advanced function which make font changes to substrings of str.
;; Surround substrings of str, as specified by the third parameter, in font tags.
;; Region-color-list is a list of coloring descriptors.  <p>
;; Each color descriptor is of the form:
;; (from-string to-string color face multiplicity). <p>
;; Face and multiplicity are optional.
;; From-string and to-strings delimits and identifies a substring of str to colorize etc.
;; color is a list of three integers: a rgb list.
;; We support the following face symbols: italic, bold, typewriter, underlined, plain (default bold).
;; Multiplicity is an integer telling how many times to to attempt the colorization on str (default 1). <p>
;; NB: In strange situations, the fontification of an early region-color-element may
;; affect the searching for latter region-color-elements. This is not an error, but a consequence of
;; the way font tags are puted into str.
(define (colorize-substrings str region-color-list)
  (set! last-coloring-length 0)
  (if (null? region-color-list)
      str
      (let* ((region-color (car region-color-list))
             (from-str (car region-color))
             (to-str (cadr region-color))
             (color (caddr region-color))
             (face (if (>= (length region-color) 4) (cadddr region-color) 'bold))
             (multiplicity (if (>= (length region-color) 5) (fifth region-color) 1))
            )
        (colorize-substrings
          (font-substring str 0 from-str to-str color face multiplicity)
          (cdr region-color-list)))))

; Return a face start tag of a given face symbol. 
; We support the following face symbols: italic, bold, typewriter, underlined, plain
(define (face-start-tag face-symbol)
  (cond ((eq? face-symbol 'italic) (start-tag 'i) )
        ((eq? face-symbol 'bold) (start-tag 'b))
        ((eq? face-symbol 'typerwriter) (start-tag 'kbd))
        ((eq? face-symbol 'underlined) (start-tag 'u))
        ((eq? face-symbol 'plain) "")
        (else (error "face start tag: Unknown face symbol"))
  )
)

; Return a face end tag of a given face symbol.
(define (face-end-tag face-symbol)
  (cond ((eq? face-symbol 'italic) (end-tag 'i))
        ((eq? face-symbol 'bold) (end-tag 'b))
        ((eq? face-symbol 'typerwriter) (end-tag 'kbd))
        ((eq? face-symbol 'underlined) (end-tag 'u))
        ((eq? face-symbol 'plain) "")
        (else (error "face end tag: Unknown face symbol"))
  )
)


; holds the length of font text from last substitution
(define last-coloring-length 0)
                               

(define (repeat-colorizing str start-index from-str to-str color face n)
  (if (> n 0)
      (font-substring str start-index from-str to-str color face n)
      str))
             
; surround a substring, delimited by from-delimiting-string and to-delimiting-string, by a html font tag
; with a color attribute.
; starting looking for delimiting strings at from-index
(define (font-substring str start-index from-delimiting-string to-delimiting-string color face multiplicity)
  (let ((from-index (substring-index str start-index from-delimiting-string)))
   (if from-index
       (let ((to-index (substring-index str 
                           (+ from-index (string-length from-delimiting-string))  ; addition 10.9.98
                           to-delimiting-string)))
          (if to-index
              (repeat-colorizing 
                 (font-substring-by-index str from-index (+ to-index (string-length to-delimiting-string)) color face)
                 (+ to-index last-coloring-length) from-delimiting-string to-delimiting-string color face (- multiplicity 1))
              (error (string-append "Substring fonting/colorizing: Cannot find the to delimiting strings: "
                                       to-delimiting-string " in " (initial-prefix-of-string str 40) ))))
        (error (string-append "Substring fonting/colorizing: Cannot find the from delimiting strings: "
                               from-delimiting-string " in " (initial-prefix-of-string str 40))))))

(define (initial-prefix-of-string str n)
 (let ((lgt (string-length str)))
   (if (> lgt n)
       (substring str 0 n)
       str)))   


; to-index is larger than from-index.
; insert a font tag around index range
(define (font-substring-by-index str from-index to-index color face)
 (let* ((pre (string-append (face-start-tag face) (start-tag 'font 'color (rgb-color-encoding color))))
        (post (string-append (end-tag 'font) (face-end-tag face)))
       )
   (set! last-coloring-length (+ (string-length pre) (string-length post)))
   (put-around-substring
      str from-index pre to-index post)))



; --------------------------------------------------------------------------------------------------------
;;; Miscelaneous

;; Embed x into a copyright indication
(define (copyright-owner x) (span x " " copyright))

;; Return a string that describes the 'last modfied' or 'page created' time.
;; This abstraction is an XML-in-LAML abstraction which accepts the attributes updated-as-of or new-as-of.
;; If the updated-as-of attribute is present, the last modfied string is returned.
;; If the new-as-of attribute is present, the page created string is returned.
;; Else the empty string is returned
;; .reference "similar function in time.scm" "when-generated" "time.html#when-generated"
;; .attribute updated-as-of implied The attribute value is a string of the form "year-month-day"
;; .attribute new-as-of implied The attribute value is a string of the form "year-month-day"
(define when-modified
  (xml-in-laml-abstraction 
    (lambda (c a)
      (let ((when-updated (defaulted-get-prop 'updated-as-of a #f))
            (when-created (defaulted-get-prop 'new-as-of a #f)))
        (cond  (when-updated 
                  (let* ((updated-date-list (year-month-day-decode-string when-updated))
                         (updated-time (time-encode (first updated-date-list) (second updated-date-list) (third updated-date-list) 0 0 0))
                         (dt (date-time updated-time)))
                    (string-append "Last modified: " (weekday updated-time) ", " (car dt))))
               (when-created
                  (let* ((created-date-list (year-month-day-decode-string when-created))
                         (created-time (time-encode (first created-date-list) (second created-date-list) (third created-date-list) 0 0 0))
                         (dt (date-time created-time)))
                    (string-append "Page created: " (weekday created-time) ", " (car dt))))
               (else ""))))))

