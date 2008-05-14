;=>man/general.sdoc

;;;; .title Reference Manual of the General LAML library
;;;; .schemedoc-dependencies  "man/color" "man/time" "compatibility/man/mzscheme-compat"
;;;; This is a library of common and generally useful Scheme functions, which are used in other LAML libraries,
;;;; in LAML styles, and in LAML tools. Far the majority of the functions can also be used outside LAML. 

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

; ---------------------------------------------------------------------------------------------------

;;; Optional parameter handling.
;;; Given the function <kbd>(lambda (r1 r2 . optional-parameters) ...)</kbd> the function
;;; <kbd>optional-parameter</kbd> (see below) is able to extract optional parameter number n. Non-used optional parameter can
;;; either be passed as the #f value (false in Scheme) or not passed at all. 
;;; .section-id optional-parameter-section

;; Return element n of optional-parameter-list. The first element is number 1.
;; In Scheme the optional parameters are captured as a list after the required parameters: <kbd>(define f (x y . optional-parameter-list) ...)</kbd>.
;; Please notice that if you pass optional parameter number i, the optional parameters 1, 2, ..., i-1 must be passed explicitly.
;; If you explicitly pass the symbol non-passed-value, this function will always return the default value, default-value.
;; (This means, of course, that you cannot use the symbol non-passed-value as an 'ordinary value' in your program).
;; If no optional third parameter - default-value - is given to the function optional-parameter the value #f serves as the default default-value.
;; .form (optional-parameter n optional-parameter-list [default-value])
;; .pre-condition optional-parameter-list is a proper list.
(define (optional-parameter n optional-parameter-list . optional-default-value)
  (let ((optional-default-value-1 (if (null? optional-default-value) #f (car optional-default-value))))  ; the old fashioned way of handling it...
   (if (> n (length optional-parameter-list))
       optional-default-value-1
       (let ((candidate-value  (list-ref optional-parameter-list (- n 1))))
         (if (eq? candidate-value 'non-passed-value)
             optional-default-value-1
             candidate-value))))) 


;;; .section-id selection-generation
;;; List selection functions and their generators. 
;;; As an alternative to using car, cadr etc. we provide for generation of more general list selector functions.


;; Returns a function, which selects element number n in a list.
;; The second parameter, which is optional, is used for error message purposes.
;; In general, this parameter should be a string corresponding to the name of the selector function.
;; If the second parameter is given, we check whether the list is long enough for selection.
;; If not, we give a decent error message. We recommend use of the second parameter in order to
;; avoid meaningless error messages.
;; The first element is number 1.
;; (make-selector-function 1) corresponds to car, (make-selector-function 2) corresponds to cadr, etc.
;; .form (make-selector-function n [selector-name])
(define (make-selector-function n . optional-parameter-list)
 (let ((selector-name (optional-parameter 1 optional-parameter-list #f)))
   (if selector-name
       (lambda (lst) 
         (cond ((list? lst)
                 (let ((lgt (length lst)))
		   (if (> n lgt)
		       (display-error (string-append "The selector function " (as-string selector-name) ": " 
						     "The list "  (as-string lst) " is is too short for selection. "
						     "It must have at least " (as-string n) " elements."
						     ))
		       (list-ref lst (- n 1)))))
               (else (display-error (string-append "The selector function " (as-string selector-name) ": " 
						   "The parameter  "  (as-string lst) "  is supposed to be a list. "
                                                   "In addition, it must have at least "
				                    (as-string n) " elements."
						     )))))
       (lambda (lst) (list-ref lst (- n 1))))))

;; Make and return a mutator function which mutates element number n in a list.
;; The returned function takes a list and a new value as arguments.
;; This function takes one optional parameter, which is the name of the mutator.
;; .form (make-mutator-function n [mutator-name])
;; .parameter n The position in the last to be mutated. The first element counts as number 1.
;; .parameter mutator-name The name of the mutator function. Used only for error message purposes.
;; .returns A function of two parameters (lambda (lst new-value) ...) that mutates element n of lst to new-value.
(define (make-mutator-function n . optional-parameter-list)
 (let ((mutator-name (optional-parameter 1 optional-parameter-list)))
   (if mutator-name
       (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "The mutator function " (as-string mutator-name) ": " 
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."
                               ))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value)))))
      (lambda (lst new-value) 
         (let ((lgt (length lst)))
            (if (> n lgt)
                (display-error (string-append "Error in mutator:"
                                 "The list "  (as-string lst) " is is too short for mutator. "
                                 "It must have at least " (as-string n) " elements."))
                (let ((cons-pair (list-tail lst (- n 1))))
                  (set-car! cons-pair new-value))))))))


;; Return the first element of a list
;; .form (first lst)
(define first car)

;; Return the second element of a list
;; .form (second lst)
(define second cadr)

;; Return the third element of a list
;; .form (third lst)
(define third caddr)

;; Return the fourth element of a list
;; .form (fourth lst)
(define fourth cadddr)

;; Return the fifth element of a list
;; .form (fifth lst)
;; .returns The fifth element of the list
(define fifth (make-selector-function 5))

;; Return the sixth element of a list
;; .form (sixth lst)
(define sixth (make-selector-function 6))

;; Return the seventh element of a list
;; .form (seventh lst)
(define seventh (make-selector-function 7))

;; Return the eighth element of a list
;; .form (eighth lst)
(define eighth (make-selector-function 8))

;; Return the nineth element of a list
;; .form (nineth lst)
(define nineth (make-selector-function 9))


;;; Association and property list functions. 
;;; Here follows a number of functions which work on alists, or make alists. Also a number of property list functions are provided.

;; Add a key-value pair to a-list. Like acons in some systems.
(define (extend-a-list key value a-list)
 (cons (cons (as-symbol key) value) a-list))

;; Return a value from an alist which corresponds to key.
;; In case the key does not exist in the alist, a fatal error will occur.
;; .parameter key is a symbol.
;; .parameter a-list an association list with symbols as keys.
;; .returns the first value of key in a-list.
;; .misc Uses the function assq (based on eq? for key comparions) internally.
;; .internal-references "similar function" "defaulted-get"
(define (get key a-list)
  (let ((res (assq key a-list)))
    (if (pair? res) 
        (cdr res)
        (error (string-append "Get: Cannot find " (as-string key) " in " (as-string a-list))))))

;; Return the value of key in alist (by means of cdr of assq). If no association is found return default.
;; .internal-references "similar function" "get"
(define (defaulted-get key alist default)
  (let ((res (assq key alist)))
    (if res
       (cdr res)
       default)))


;; Return the value of key in the property list p-list.
;; In case the key does not exist in the property list, a fatal error will occur.
;; .parameter key is a symbol.
;; .parameter p-list a property list with symbols as keys.
;; .returns the first value of key in p-list
;; .misc Uses the function eq? for key comparions.
;; .internal-references "similar function" "defaulted-get-prop"
;; .pre-condition p-list is of even length
(define (get-prop key p-list)
  (let ((res (find-in-property-list key p-list)))
    (if res
        (if (not (null? (cdr res)))
            (cadr res)
            (laml-error "Illformed property list:" (as-string p-list)))
        (laml-error "Get-prop: Cannot find" (as-string key) "in the property list" (as-string p-list)))))

;; Does key exists a property name in the property list p-list?
;; If so, return a reference to the cons cell that holds key.
(define (find-in-property-list key p-list)
  (cond ((null? p-list) #f)
        ((eq? key (car p-list)) p-list)
        ((not (null? (cdr p-list))) (find-in-property-list key (cddr p-list)))
        (else (laml-error "Illformed property list:" (as-string p-list)))))

;; Return the value of key in the property list p-list. If key is not present in p-list, return default.
;; .internal-references "similar function" "get-prop"
;; .pre-condition p-list is a list of even length
(define (defaulted-get-prop key p-list default)
  (let ((res (find-in-property-list key p-list)))
    (if res
        (if (not (null? (cdr res)))
            (cadr res)
            (laml-error "Illformed property list:" (as-string p-list)))
        default)))

;; Remove all instances of key and its values form the property list p-list. 
;; .returns the resulting, possible shorter, property list (a reduced copy of p-list).
;; .pre-condition p-list is a well-formed property list.
;; .misc This is a function, not a mutating procedure.
(define (remove-prop! key p-list)
  (cond ((null? p-list) '())
        ((eq? key (car p-list))
            (remove-prop! key (cddr p-list)))
        (else 
            (cons (car p-list) (cons (cadr p-list) (remove-prop! key (cddr p-list)))))))

;; Remove all instances of keys in key-list together with its values form the property list p-list. 
;; .returns the resulting, possible shorter, property list (a reduced copy of p-list).
;; .pre-condition p-list is a well-formed property list.
;; .misc This is a function, not a mutating procedure.
(define (remove-props! key-list p-list)
  (cond ((null? p-list) '())
        ((memq (car p-list) key-list)
            (remove-props! key-list (cddr p-list)))
        (else 
            (cons (car p-list) (cons (cadr p-list) (remove-props! key-list (cddr p-list)))))))

;; Make an alist from a key-list and a val-list. 
;; .pre-condition the lengths of the two input lists are equal.
(define (alist-from-keys-and-values key-list val-list)
  (if (= (length key-list) (length val-list))
      (alist-from-keys-and-values-1 key-list val-list)
      (error "alist-from-keys-and-values: key and val list do not have same lengths")))

(define (alist-from-keys-and-values-1 key-list val-list)
  (if (null? key-list)
      '()
      (cons (cons (car key-list) (car val-list))
            (alist-from-keys-and-values-1 (cdr key-list) (cdr val-list)))))

;; Make and return an association list from a property list plist.
(define (propertylist-to-alist plist)
  (let ((lgt (length plist)))
   (cond ((null? plist) '())
         ((= 1 lgt) (error "propertylist-to-a-list called with list of odd length. A property list is always of even length"))
         ((>= lgt 2) (cons (cons (car plist) (cadr plist)) (propertylist-to-alist (cddr plist)))))))

;; Make and return a property list from an association list.
(define (alist-to-propertylist alist)
  (cond ((null? alist) '())
        (else (cons (car (car alist)) (cons (cdr (car alist)) (alist-to-propertylist (cdr alist)))))))

;; Return every second element of list, starting with the first element.
;; This function is useful to extract the keys or values of a property list.
(define (every-second-element lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        (else (cons (car lst) (every-second-element (cddr lst))))))

;; Return those property names and values of prop-list which are not in eliminations.
;; .parameter prop-list A well-formed property list, in which the property names are symbols.
;; .parameter eliminations A list of property names, where each property name is a symbol.
(define (but-props prop-list eliminations)
 (but-props-1 prop-list eliminations '()))

(define (but-props-1 prop-list eliminations res)
 (cond ((null? prop-list) (reverse res))
       ((null? (cdr prop-list)) (laml-error "but-props called with ill-formed property list (odd number of elements)"))
       (else (let ((name (car prop-list))
                   (val (cadr prop-list)))
               (if (memq name eliminations) 
                   (but-props-1 (cddr prop-list) eliminations res)
                   (but-props-1 (cddr prop-list) eliminations (cons val (cons name res))))))))

;; Return those property key/value pairs of the proper list prop-list whose keys are member of keylist.
;; Comparison is done by the function memq.
(define (property-subset prop-list keylist)
  (cond ((null? prop-list) '())
        ((memq (car prop-list) keylist)
         (cons (car prop-list) (cons (cadr prop-list) (property-subset (cddr prop-list) keylist))))
        (else (property-subset (cddr prop-list) keylist))))


;; Return a list of pairs of elements from lst1 and lst2.
;; In other words, return an association list with keys from lst1 and values from lst2.
;; The list is as long as the shortest of lst1 and lst2.
(define (pair-up lst1 lst2)
  (pair-up-1 lst1 lst2 '()))

(define (pair-up-1 lst1 lst2 res)
  (cond ((or (null? lst1) (null? lst2)) (reverse res))
        (else (pair-up-1 (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) res)))))

;; A function which converts the key position in an a-lists to a symbol.
;; .parameter key-value-pair a pair, such as ("key" . "val")
;; .returns a pair (key . "val")
(define (symbolize-key key-value-pair)
 (cons (as-symbol (car key-value-pair)) (cdr key-value-pair)))


;;; Filter and accumulation functions. 
;;; This sections provides variants of the very useful higher order filtering function.

;; Filter a list lst by means of the predicate pred. Preserves the ordering of elements in lst.
;; .returns the elements in lst that fulfills the predicate pred.
;; .misc Based on a tail recursive traversal of lst.
;; .internal-references "similar function" "filter-no-ordering"
(define (filter pred lst)
  (reverse (filter-help pred lst '())))

;; Like filter, but the ordering among elements in the resulting list is unknown and arbitrary.
;; Actually returns filtered list in reverse order. OK in situations,
;; where a boolean result is needed: Are there anything passing the filter?
;; .internal-references "similar function" "filter"
(define (filter-no-ordering pred lst)
  (filter-help pred lst '()))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) (filter-help pred (cdr lst) (cons (car lst) res)))
        (else (filter-help pred (cdr lst) res))))


; October 5, 2005: mapping-filter generalized to several lists (of equal lengths).

;; Map and filter the lists in the parameter lists by means of the predicate pred.
;; In the same ways as for the native map function of Scheme, lists must be a list of lists, and pred must be a function taking as many arguments as there are lists and returning a single value. If more than one list is given, then they must all be the same length. 
;; If the predicate pred returns a true value v on the elements in lists, return v instead of e (this is the mapping effect). 
;; Only return those mapped elements that fullfil pred.
;; .misc Remember that any non-#f element counts as the true (#t) value.
(define (mapping-filter pred . lists)
  (reverse (mapping-filter-help pred lists '())))

(define (mapping-filter-help pred lists res)
  (if (null? (car lists))
      res
      (let ((pred-appl (apply pred (map car lists))))
        (if pred-appl 
            (mapping-filter-help pred (map cdr lists) (cons pred-appl res))
            (mapping-filter-help pred (map cdr lists) res)))))

; accumulate-right contributed by ttn@giblet.glug.org, November 28, 2002.

;; A higher-order function which right accumulates the list lst by means of the binary function f, 
;; using init as the initial value of the accumulation.
;; .misc This function is iterative.
(define (accumulate-right f init lst)
  (let loop ((lst (reverse lst)) (acc init))
    (if (null? lst)
	acc
	(loop (cdr lst) (f (car lst) acc)))))


;;; Mapping functions.
;;; Here is a set of generalized mapping functions. These functions are all similar to map (which may take an arbitrary number of lists).
;;; Notice however, that map2, map3, etc do not require all lists to be of equal lengths.

;; Like map, but maps f on two lists. 
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map2 f lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (f (car lst1) (car lst2)) 
            (map2 f (cdr lst1) (cdr lst2)))))

;; Like map, but maps f on three lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map3 f lst1 lst2 lst3)
  (if (or (null? lst1) (null? lst2) (null? lst3)) '()
      (cons (f (car lst1) (car lst2) (car lst3)) 
            (map3 f (cdr lst1) (cdr lst2) (cdr lst3)))))

;; Like map, but maps f on four lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map4 f lst1 lst2 lst3 lst4)
  (if (or (null? lst1) (null? lst2) (null? lst3)  (null? lst4)) '()
      (cons (f (car lst1) (car lst2) (car lst3) (car lst4)) 
            (map4 f (cdr lst1) (cdr lst2) (cdr lst3) (cdr lst4)))))

;; Like map, but maps f on five lists
;; .returns Returns a list of length equal to the length of the shortest of the input lists.
(define (map5 f lst1 lst2 lst3 lst4 lst5)
  (if (or (null? lst1) (null? lst2) (null? lst3)  (null? lst4) (null? lst5)) '()
      (cons (f (car lst1) (car lst2) (car lst3) (car lst4) (car lst5) ) 
            (map5 f (cdr lst1) (cdr lst2) (cdr lst3) (cdr lst4) (cdr lst5)))))



;;; Other higher-order functions.

;; A higher order functions which negates the predicate p. Negate accepts a predicate and returns the negated predicate.
(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

;; Form the disjunction (logical or) of the two predicates p and q.
;; .returns a predicate
(define (disjunction p q)
  (lambda (x)
    (or (p x) (q x))))

;; Form the conjunction (logical and) of the two predicates p and q.
;; .returns a predicate
(define (conjunction p q)
  (lambda (x)
    (and (p x) (q x))))

; Old version of compose:
; Return a composed function which applies f on g
; Both f and g are supposed to take a single argument.
;(define (compose f g)
;  (lambda (x)
;    (f (g x))))

;; Compose a list of functions to a single function.
;; Each function in the list takes a single parameter.
;; Handles the typical case of two functions manually to achieve better efficiency.
;; .pre-condition f-list is a proper list of length ast least one.
(define (compose . f-list)
  (cond ((= 1 (length f-list)) (car f-list))
        ((= 2 (length f-list))
	 (let ((f (car f-list)) 
	       (g (cadr f-list)))
	   (lambda (x) (f (g x)))))
        (else  (lambda (x)
		 ((car f-list)
		  ((apply compose (cdr f-list)) x))))))


;; Generate a less than or equal predicate from the enumeration-order.
;; If p is the generated predicate, (p x y) is true if and only if
;; (selector x) comes before (or at the same position) as (selector y)
;; in the enumeration-order. Thus, (selector x) is assumed to give a
;; value in enumeration-order. Comparison with elements in the enumeration-list
;; is done with el-eq?
;; .form (generate-leq enumeration-order selector [el-eq?])
(define (generate-leq enumeration-order selector . optional-parameter-list)
 (let ((el-eq? (optional-parameter 1 optional-parameter-list eq?)))
  (lambda (x y)
     ; x and y supposed to be elements in enumeration order
     (let ((x-index (list-index (selector x) enumeration-order el-eq?))
           (y-index (list-index (selector y) enumeration-order el-eq?)))
       (<= x-index y-index)))))

; A helping function of generate-leq.
; Return the position of e in lst. First is 1
; compare with el-eq?
; if e is not member of lst return (+ 1 (length lst))
(define (list-index e lst el-eq?)
 (cond ((null? lst) 1)
       ((el-eq? (car lst) e) 1)
       (else (+ 1 (list-index e (cdr lst) el-eq?)))))

;; Generalize f with ad hoc currying. 
;; f is a function which, in its native form, takes two or more parameters.
;; The generalization allows f to act as a curried function. In case (curry-generalized f)
;; only receives a single parameter, it returns a lambda function which waits for the 
;; remaining parameters.
;; If two or more parameters are passed to f, f is applied on the parameters; In this case
;; (curry-generalized f) is equivalent to f.
;; .example (define gmap (curry-generalized map))
;; .example (define gfilter (curry-generalized filter))
(define (curry-generalized f)
  (lambda rest
    (cond ((= (length rest) 1) (lambda lst (apply f (cons (car rest) lst))))
          ((>= (length rest) 2) (apply f rest)))))

;;; List and Sexpr functions.

;; Return a list of all numbers from f to t. Return the empty list if f is greater than t.
(define (number-interval f t)
 (if (<= f t) (cons f (number-interval (+ f 1) t)) '()))


;; Return the proper part of an S-expression
(define (proper-part lst)
  (cond ((and (pair? lst) (pair? (cdr lst)))  (cons (car lst) (proper-part (cdr lst))))
        ((pair? lst) (cons (car lst) '()))
        (else '())))

;; Return the first improper part of an S-expression
(define (first-improper-part lst)
  (cond ((and (pair? lst) (pair? (cdr lst)))  (first-improper-part (cdr lst)))
        ((pair? lst) (cdr lst))
        (else (error (string-append "Troubles in first-improper-part:" (as-string lst))))))



;; Return a list of n elements, each being el
(define (make-list n el)
  (if (<= n 0) '() (cons el (make-list (- n 1) el))))

;; Replicate lst cyclically to a list of length lgt
(define (replicate-to-length lst lgt)
  (reverse (replicate-to-length-1 lst lst '() 0 lgt)))

; helping function to replicate-to-length
; original-lst is constant through this function.
; elements are taken out of lst
; the result is accumulated up in res
; count goes from 0 to lgt
(define (replicate-to-length-1 original-lst lst res count lgt)
 (cond ((null? lst) (replicate-to-length-1 original-lst original-lst res count lgt))
       ((< count lgt) (replicate-to-length-1 original-lst (cdr lst) (cons (car lst) res) (+ 1 count) lgt))
       (else res)))


;; Flatten a list of lists to one list.
(define (flatten lst-of-lst)
  (accumulate-right append '() lst-of-lst))

;; Add all elments in a list of numbers
(define (sum-list lst)
  (accumulate-right + 0 lst))



;; Merge list1 and list2. Let e1 be the head of list1 and e2 the head of list2.
;; take e2 if (pred e1 e2) holds. Else e1
(define (merge-lists list1 list2 pred)
 (cond ((null? list1) list2)
       ((null? list2) list1)
       ((pred (car list1) (car list2)) (cons (car list2) (merge-lists list1 (cdr list2) pred)))
       (else (cons (car list1) (merge-lists (cdr list1) list2 pred)))))

;; Merge the two lists lst1 and lst2. lst1 provides the first element.
;; When the shortets of the lists is exhausted, insert the rest of the other list.
;; .example (merge-lists-simple '(a b c d)  '(1 2 3))  => (a 1 b 2 c 3 d)
(define (merge-lists-simple lst1 lst2)
 (merge-lists-simple-1 lst1 lst2 '()))

(define (merge-lists-simple-1 lst1 lst2 res)
  (cond ((null? lst1) (reverse (append (reverse lst2) res)))
        ((null? lst2) (reverse (append (reverse lst1) res)))
        (else (merge-lists-simple-1 (cdr lst1) (cdr lst2) (cons (car lst2) (cons (car lst1) res ))))))

;; A simple linear list search function.
;; Return the first element which satisfies the predicate pred.
;; If no such element is found, return #f.
;; Tail recursive and iterative.
;; .internal-references "Similar string function" "find-in-string-by-predicate"
(define (find-in-list pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-in-list pred (cdr lst)))))

;; Return the list of all cons cells reachable from cell which satisfy pred.
;; If a cell c is accepted by the predicate, the cells of (cdr cell) are also examined for matches.
;; .parameter pred a cons-cell predicate.
;; .parameter cell a pair (such as a list).
;; .pre-condition cell is a cons cell (satisfies pair?)
(define (traverse-cons-cells pred cell)
  (cond ((not (pair? cell)) '())
        ((pred cell) (cons cell (traverse-cons-cells pred (cdr cell))))
        ((and (pair? (car cell)) (pair? (cdr cell)))
            (append (traverse-cons-cells pred (car cell)) (traverse-cons-cells pred (cdr cell))))
        ((pair? (car cell))
            (traverse-cons-cells pred (car cell)))
        ((pair? (cdr cell))
            (traverse-cons-cells pred (cdr cell)))
        (else '())))

;; Return all but the last element of a list. Quick and dirty version.
(define (butlast lst)
  (reverse (cdr (reverse lst))))

;; Return the last element of a list. Quick and dirty version.
(define (last lst)
  (car (reverse lst)))

;; Remove duplicate elements from list. A non-destructive function.
;; This function uses the Scheme function equal? (via the Scheme function member) for comparison of elements.
;; .example (remove-duplicates '(1 2 3 4 5)) => (1 2 3 4 5)
;; .example (remove-duplicates '(1 2 3 2 3 4 5)) => (1 2 3 4 5)
(define (remove-duplicates lst)
 (remove-duplicates-help lst '()))

(define (remove-duplicates-help lst res)
  (cond ((null? lst) (reverse res))
        ((member (car lst) res) (remove-duplicates-help (cdr lst) res))
        (else (remove-duplicates-help (cdr lst) (cons (car lst) res)))))

;; A variant of remove-duplicates with a selector function.
;; This function applies a selector function before comparisons and member is called.
;; This function uses equal? for comparison of elements.
(define (remove-duplicates-with-selection lst selector)
 (remove-duplicates-with-selection-help lst '() '() selector))

(define (remove-duplicates-with-selection-help lst res selected-res selector)
  (cond ((null? lst) (reverse res))
        ((member (selector (car lst)) selected-res)
            (remove-duplicates-with-selection-help (cdr lst) res selected-res selector))
        (else (remove-duplicates-with-selection-help (cdr lst) (cons (car lst) res)  (cons (selector (car lst)) selected-res) selector ))))

;; Return the element of lst just before el, or #f if no such element exists.
;; More precisely, return the element of lst just before the element e, where e contains el in the sense that (eq? (selector e) el).
;; Via use of the optional parameter, comparison can be done by use of another function than eq?.
;; .form (element-before el lst selector [eq-pred])
;; .parameter el The element constituent that we are looking for.
;; .parameter lst The list to search.
;; .parameter selector A function that selects a constituent of an element of the list.
;; .parameter eq-pred The equality predicate on list constituents.\
;;  Can be used to compare el with (selector e) for any element e in lst. Defaults to eq?.
;; .returns An element of the list lst, preceding the element containing el.\
;;  Or #f in case such an element does not exist.
(define (element-before el lst selector . optional-parameter-list)
 (let ((eq-pred (optional-parameter 1 optional-parameter-list eq?)))
  (element-before-1 el lst selector (length lst) eq-pred)))

(define (element-before-1 el lst selector lgt eq-pred)
  (cond ((<= lgt 1) #f)
        ((eq-pred el (selector (car lst))) #f)
        ((eq-pred el (selector (cadr lst))) (car lst))
        (else (element-before-1 el (cdr lst) selector (- lgt 1) eq-pred))))

;; Return the element of lst just after el, or #f if no such element exists.
;; More precisely, return the element of lst just after the element e, where e contains el in the sense that (eq? (selector e) el).
;; Via use of the optional parameter, comparison can be done by use of another function than eq?.
;; .form (element-after el lst selector [eq-pred])
;; .parameter el The element constituent that we are looking for.
;; .parameter lst The list to search.
;; .parameter selector A function that selects a constituent of an element of the list.
;; .parameter eq-pred The equality predicate on list constituents. Can be used to compare el with (selector e) for any element e in lst. Defaults to eq?.
;; .returns An element of the list lst, following the element containing el. Of #f in case such an element does not exist.
(define (element-after el lst selector . optional-parameter-list)
 (let ((eq-pred (optional-parameter 1 optional-parameter-list eq?)))
  (element-after-1 el lst selector (length lst) eq-pred)))

(define (element-after-1 el lst selector lgt eq-pred)
  (cond ((<= lgt 1) #f)
        ((eq-pred el (selector (car lst))) (cadr lst))
        (else (element-after-1 el (cdr lst) selector (- lgt 1) eq-pred))))


;; Remove the elements of lst2 from lst1. 
;; This function is a non-destructive function.
;; .form (list-difference lst1 lst2 [is-eq?])
;; .parameter lst1 The list from which lst1 is subtracted
;; .parameter lst2 The list to subtract from lst1
;; .parameter is-eq? the equalilty function used for element comparison. The default comparison function is eq?
;; .returns The elements in lst1 which are not member of lst2
(define (list-difference lst1 lst2 . optional-parameter-list)
 (let ((is-eq? (optional-parameter 1 optional-parameter-list eq?)))
  (list-difference-1 lst1 lst2 '() is-eq?)))

(define (list-difference-1 lst1 lst2 res eq-pred)
  (cond ((null? lst1) (reverse res))
        ((member-by-predicate (car lst1) lst2 eq-pred) (list-difference-1 (cdr lst1) lst2 res eq-pred))
        (else (list-difference-1 (cdr lst1) lst2 (cons (car lst1) res) eq-pred))))


;; Return a list of lists of elements from lst.
;; Each sub list is of length n.
;; Take elements consequtive (by rows) and put them into sublists.
;; .internal-references "More general function" "sublist-by-predicate"
(define (sublist-by-rows n lst)
 (let ((lgt (length lst)))
  (cond ((<= n 0) (error  (string-append "sublist-by-rows: Cannot deal with row numbers less than or equal to zero: " (as-string n))))
        ((< lgt n) (list lst))
        (else (sublist-by-rows-1 n lst 0 '() '())))))

(define (sublist-by-rows-1 n lst m res RESULT)
  (cond ((and (null? lst) (null? res)) (reverse RESULT))  ;@a
        ((and (null? lst) (not (null? res))) (reverse (cons (reverse res) RESULT)))    ;@b
        ((= m n ) (sublist-by-rows-1 n lst 0 '() (cons (reverse res) RESULT)))         ;@c
        ((<= m n) (sublist-by-rows-1 n (cdr lst) (+ m 1) (cons (car lst) res) RESULT)) ;@d
        (else (error "sublist-by-rows-1: Should not happen"))))                       

;; Return sublists of lst in two column format. Thus each produced sublist is of length 2.
;; Good for presentation of the list in two columns, column by column.
;; In cases there is an uneven number of elements in lst, we add extra (the second parameter).
(define (sublist-by-2columns lst extra)
 (if (null? lst)
     '()
  (let* ((lgt (length lst))
         (lst1 (if (even? lgt) lst (append lst (list extra))))
         (row-sublst (sublist-by-rows (quotient (if (even? lgt) lgt (+ 1 lgt)) 2) lst1)) ; @i
        )
    (map ; @j
      (lambda (e1 e2) (list e1 e2))
      (car row-sublst) (cadr row-sublst)))))

;; Return sublists of lst in an n column format. Thus each produced sublist is of length n
;; (the first parameter).
;; In cases there is not enough elements, we add extra (the last parameter).
(define (sublist-by-columns n lst extra)
 (if (null? lst)
     '()
  (let* ((lgt (length lst))
         (q (quotient lgt n))
         (lst1 (if (multiplum-of lgt n) lst (append lst (make-list (- (* (+ q 1) n) lgt) extra)))) ; @a
         (rows (if (multiplum-of lgt n) q (+ q 1)))
         (row-sublst (sublist-by-rows rows lst1)))
    (multi-pair row-sublst))))

;; Pair up first elements, second elements of a list of lists.
;; All first elements of the sublists are handled first, whereafter
;; we take all second elements, etc. 
;; .parameter lst-of-lst A list of lists.
;; .pre-condition All lists in lst-of-list are of equal lengths.
(define (multi-pair lst-of-lst)
  (cond ((null? (car lst-of-lst)) '())
        (else (let ((cars (map car lst-of-lst))
                    (cdrs (map cdr lst-of-lst)))
                (cons cars (multi-pair cdrs))))))

;; Return a list of sublists of elements from lst controlled by a predicate p.
;; The sublists are formed by examining elements from lst.  The predicate p decides
;; when to start a new sublist. Thus, when p evaluates to true (on some element e and its preceding element c) we start
;; a new sublist (whose first element becomes e). The predicate p is not activated on (car lst). 
;; This function generalizes sublist-by-rows.
;; .parameter lst An arbitrary list.
;; .parameter p A predicate of the form (lambda (cur prev n) ...) where cur is the current element, prev is the preceding element of cur,\
;;            and n is the number of elements preceding cur in the original list lst.
(define (sublist-by-predicate lst p)
  (cond ((null? lst) '()) ;@a
        ((= 1 (length lst)) (list lst))  ; @b special case: sublist the only element.
        (else (sublist-by-predicate-1 (cdr lst) (car lst) p 1 (list (car lst)) '()))))


(define (sublist-by-predicate-1 lst previous-el p n res RESULT)
  (cond ((and (null? lst) (null? res)) (reverse RESULT)) ;@d
        ((and (null? lst) (not (null? res))) (reverse (cons (reverse res) RESULT))) ;@e
        ((p (car lst) previous-el n)   (sublist-by-predicate-1 (cdr lst) (car lst) p (+ n 1) (list (car lst)) (cons (reverse res) RESULT))) ;@f
        (else                          (sublist-by-predicate-1 (cdr lst) (car lst) p (+ n 1) (cons (car lst) res) RESULT)))) ;@g

;; Remove duplicates from lst.
;; A pair of duplicates satisfy the predicate p: (p element element) -> boolean.
;; In case of duplicates, keep the first one in the result.
(define (remove-duplicates-by-predicate lst p)
 (remove-duplicates-by-predicate-1 lst p '()))

(define (remove-duplicates-by-predicate-1 lst p res)
  (cond ((null? lst) (reverse res))
        ((member-by-predicate (car lst) res p) (remove-duplicates-by-predicate-1 (cdr lst) p res))
        (else (remove-duplicates-by-predicate-1 (cdr lst) p (cons (car lst) res)))))

;; Return the duplicates in lst. 
;; The duplicates are returned in the order of their fist occurence in lst.
;; Comparison of elements is done by the predicate (p element element) -> boolean.
(define (duplicates-by-predicate lst p)
  (duplicates-by-predicate-1 lst p '()))

(define (duplicates-by-predicate-1 lst p res)
  (cond ((null? lst) (reverse res))
        ((member-by-predicate (car lst) (cdr lst) p)
            (if (member-by-predicate (car lst) res p)        ; always detected as duplicate once
                (duplicates-by-predicate-1 (cdr lst) p res)
                (duplicates-by-predicate-1 (cdr lst) p (cons (car lst) res))))
        (else (duplicates-by-predicate-1 (cdr lst) p res))))  

;; Is el member of lst by means of the predicate p.
;; el is always passed as the first parameter to p.
;; If el is member, return the suffix of the list in which the first element (and el) satisfy the predicate.
;; Else return #f.
;; The element el and elements of lst are compared by p, el as the first one.
;; p: (el1, el2) -> boolean
(define (member-by-predicate el lst p)
  (cond ((null? lst) #f)
        ((p el (car lst)) lst)
        (else (member-by-predicate el (cdr lst) p))))

;; Return the elements of lst1 an lst2 which belongs to both of the lists.
;; Elements will never occur more than once in the result.
;; Element comparsion is done by pred.
;; Performance: O (length lst1) x (length lst2).
;; .parameter pred: Element x Element -> Boolean.
;; .example (list-intersection '(a b c d a) '(a d) eq?) = (a d)
(define (list-intersection-by-predicate lst1 lst2 pred)
 (list-intersection-1 lst1 lst2 pred '()))

(define (list-intersection-1 lst1 lst2 pred res)
  (cond ((null? lst1) (remove-duplicates-by-predicate (reverse res) pred))
        (else (let* ((el (car lst1))
                     (el-member-lst2 (member-by-predicate el lst2 pred)))
                (list-intersection-1 (cdr lst1) lst2 pred (if el-member-lst2 (cons el res) res))))))
  

;; Cut the tail of lst; The tail to be cutted starts with an element which fulfils pred.
;; Notice that the first element which fulfils the predicate is not included in the resulting list.
(define (cut-list-by-predicate lst pred)
  (cond ((null? lst) '())
        ((pred (car lst)) '())
        (else (cons (car lst) (cut-list-by-predicate (cdr lst) pred)))))


;; Return whether every element in set-list-1 (a list) is a member of set-list-2, compared by the comparator comp.
;; This corresponds to a subset operations on sets, represented by a list.
;; comp: el x el -> boolean.
(define (subset-of-by-predicate set-list-1 set-list-2 comp)
  (cond ((null? set-list-1) #t)
        ((member-by-predicate (car set-list-1) set-list-2 comp) (subset-of-by-predicate (cdr set-list-1) set-list-2 comp))
        (else #f)))


;; Return the index position of the fist occurrence of el in list. The first element is counted as element number 0.
;; If the element el is not in the list lst, return #f. 
;; Comparison of list elements is done by the binary comparison function c.
;; c: el x el -> boolean.
(define (index-in-list-by-predicate lst el c)
 (letrec ((index-in-list-by-predicate-1 
            (lambda (lst count)
               (cond ((null? lst) #f)
                     ((c (car lst) el) count)
                     (else (index-in-list-by-predicate-1 (cdr lst) (+ count 1)))))))
  (index-in-list-by-predicate-1  lst 0)))


;; Divide the elements of lst into sublists of sublist-length.
;; In case that sublist-length does not divide (length lst) the last
;; sublist will be shorter than the others.
;; .example (sublistify '(1 2 3 4 5 6 7 8 9) 4) = ((1 2 3 4) (5 6 7 8) (9))
(define (sublistify lst sublist-length)
 (if (<= (length lst) sublist-length)
     (list lst)
     (let ((first-sublist (list-prefix lst sublist-length))
           (rest-lst (list-tail lst sublist-length)))
       (cons first-sublist
             (sublistify rest-lst sublist-length)))))

;; Return the first n elements of lst.
;; This function makes a shallow copy of the first n elements of lst. Thus, it allocates n new cons cells.
;; If n is equal or greater than the length of lst, lst is returned without any copying at all.
;; .misc Another function, list-prefix, exists which is almost identical to front-sublist. 
(define (front-sublist lst n)
 (if (>= n (length lst)) lst (front-sublist-1 lst n)))


; A helping operation to front-sublist
(define (front-sublist-1 lst n)
 (cond ((= n 0) '())
       ((and (> n 0) (not (null? lst))) (cons (car lst) (front-sublist-1 (cdr lst) (- n 1))))
       ((and (> n 0) (null? lst)) '())
       (else (laml-error "front-sublist-1: Should not happen" lst n))))

;; Return a list prefix of lst, of which all elements satisfy the predicate ok?. 
;; The returned prefix has at most max-length elements.
;; This function makes a shallow copy of at most max-length elements of lst. Thus, it allocates a number of new cons cells.
;; .parameter lst An arbitrary list.
;; .parameter ok? A list element predicate, which is applied successively on elements of the list.
;; .parameter max-length An integer that gives the maximum number of elements to be returned by this function.
;; .returns A prefix of lst, of length at most max-length. All elements in the result satisfy the predicate ok?
(define (front-sublist-while lst ok? max-length)
  (front-sublist-while-1 lst ok? max-length   (length lst) '() 0))

(define (front-sublist-while-1 lst ok? max-length   lst-lgt res length-res)
  (cond ((= 0 lst-lgt) '())
        ((null? lst) (reverse res))
        ((= max-length length-res) (reverse res))
        ((ok? (first lst))
           (front-sublist-while-1 (cdr lst) ok? max-length
                                lst-lgt (cons (first lst) res) (+ 1 length-res)))
        (else (reverse res))))

;; Return the last n elements of lst. 
;; This function returns a reference to an appropriate tail of lst, involving only the last n elements.
;; If n is equal to or larger than (length lst), just return lst.
(define (rear-sublist lst n)
  (let ((lst-lgt (length lst)))
    (if (>= n lst-lgt)
        lst
        (let ((prefix-lgt (- lst-lgt n)))
          (list-tail lst prefix-lgt)))))


; Return the list of the first n elements of lst.
; If n > (length lst) just return lst. 
; .misc This function is almost identical to front-sublist.
(define (list-prefix lst n)
  (if (< (length lst) n)
      lst
      (list-prefix-1 lst n)))

(define (list-prefix-1 lst n)
  (if (= n 0)
      '()
      (cons (car lst) (list-prefix-1 (cdr lst) (- n 1)))))


;; Return the sublist consisting of element a to element b of the list lst.
;; Both element number a and b are included in the resulting list. The first element counts as element number 1.
;; .example (list-part 3 5 '(a b c d e f g h)) = (c d e)
;; .pre-condition a >= 1, a <= b, b <= (length lst), and a and b are postive integers.
(define (list-part a b lst)
  (list-part-help a b lst 1 (length lst) '()))

(define (list-part-help a b lst  i lgt res)
 (cond ((> i b) (reverse res))
       ((and (>= i a) (<= i b) (not (null? lst))) (list-part-help a b (cdr lst) (+ i 1) lgt (cons (car lst) res)))
       ((and (<= i a) (not (null? lst)))  (list-part-help a b (cdr lst) (+ i 1) lgt res))
       ((null? lst) (error (string-append "list-part error: " (as-string i))))))


;; The first element of the resulting list fullfils from-pred.
;; The last element is the element before the element that fullfils end-pred.
;; If from-pred is true on an element and to-pred is false on every element in list, a suffix of the list is returned.
;; In other cases, if one or both of the predicates return false on every element the empty list is returned
(define (sublist-of-list lst from-pred end-pred)
  (let ((lst-lgt (length lst))
        (i (find-index-in-list lst from-pred))
        (j (find-index-in-list lst end-pred)))
    (cond ((and i j)
            (list-part (+ i 1) j lst))
          ((and i (not j))
            (list-part (+ i 1) lst-lgt lst))
          (else '()))))

;; Find the index of the first element in lst that satisfies the element predicate pred.
;; The first element counts as number 0.
(define (find-index-in-list lst pred)
  (find-index-in-list-1 lst pred 0))

(define (find-index-in-list-1 lst pred i)
  (cond ((null? lst) #f)
        ((pred (car lst)) i)
        (else (find-index-in-list-1 (cdr lst) pred (+ i 1)))))


;;; Vector functions.

;; Search for an element el in the sorted vector v.
;; More specifically, el is compared to (sel ve), where ve is a element from the vector v.
;; Comparison is done by the binary predicate el-eq? which works on selected values.
;; Thus (el-eq? (sel x) el) makes sense for an element x in the vector.
;; Ordering in the vector is defined by the binary 'less-than-equal predicate' el-leq?
;; which compares selected values. Thus (el-leq (sel x) (sel y)) makes sense for x and y
;; being elements in the vector v.
;; .parameter v The vector to search in.
;; .parameter el The element to search for in the vector. el is comparabel with (sel ve) for a given vector element.
;; .parameter sel A function that can be applied on vector elements. 
;; .parameter el-eq? An equality function that can be applied on el and on (sel ve) for a given vector element.
;; .parameter el-leq? A less than or equal function that can be applied on el and vector elements (sel ve).
;; .returns An element in the vector, if found as described above, or #f.
(define (binary-search-in-vector v el sel el-eq? el-leq?)
 (let ((lgt (vector-length v)))
  (if (= 0 (vector-length v))
      #f
      (do ((up-idx   (- lgt 1))
	   (low-idx  0)
	   )
	  ((or (el-eq? el (sel (vector-ref v (quotient (+ up-idx low-idx) 2))))	; hit
	       (= up-idx low-idx) (= up-idx (+ 1 low-idx)) ; narrow interval
	       )
	   (cond ((el-eq? el (sel (vector-ref v (quotient (+ up-idx low-idx) 2)))) ; mid
		  (vector-ref v (quotient (+ up-idx low-idx) 2)))
		 ((el-eq? el (sel (vector-ref v low-idx))) ; low
		  (vector-ref v low-idx))
		 ((el-eq? el (sel (vector-ref v up-idx))) ; up
		  (vector-ref v up-idx))
		 (else #f)))
	(cond ((el-leq? el (sel (vector-ref v (quotient (+ up-idx low-idx) 2))))
               (set! up-idx (quotient (+ up-idx low-idx) 2)))
	      (else 
               (set! low-idx (quotient (+ up-idx low-idx) 2))))))))


;;; Conversion functions. 
;;; In this category we provide a number of useful conversion functions. Several of these are of the form (as-type xxx),
;;; where type determines the target type of the conversion.<p>
;;; This section includes a function number-in-base which converts a decimal number to a number in another number system.

;; Convert a character to a string
(define (char->string ch)
  (make-string 1 ch))


;; Convert x to a string.
;; Conversion of numbers, symbols, strings, booleans, characters, vectors, proper lists and improper lists are supported.
(define (as-string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) x)
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-string x) (make-list (- (length x) 1) " "))
               ")"))
        ((vector? x)
          (let ((lst (vector->list x)))
            (string-append "#(" 
               (string-merge (map as-string lst) (make-list (- (length lst) 1) " "))
               ")")))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-string y) " ")) (proper-part x))
               )
               " . " (as-string (first-improper-part x))
               ")"))
        (else "??")))

;; Convert x to a string, in which string constituents themselves are quoted.
;; Good for output and messages, in which strings should appear in string quotes.
(define (as-quoted-string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) (string-it x))
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-quoted-string x) (make-list (- (length x) 1) " "))
               ")"))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-quoted-string y) " ")) (proper-part x))
               )
               " . " (as-quoted-string (first-improper-part x))
               ")"))
        (else "??")))

;; Convert x to a symbol. String, symbols, booleans, and characters are supported
(define (as-symbol x)
  (cond ((symbol? x) x)
        ((string? x) (string->symbol x))
        ((boolean? x) 
            (if x (as-symbol "true") (as-symbol "false")))
        ((char? x) (as-symbol (char->string x)))
        (else #f)))

;; Convert x to a number. Strings, numbers, chars and booleans are supported.
;; Strings with digits are converted using string->number, chars are converted with char->integer, true is converted to 1, and false to 0.
(define (as-number x)
  (cond ((string? x) (string->number x))
        ((number? x) x)
        ((char? x) (char->integer x))
        ((boolean? x) (if x 1 0))  ; false -> 0, true -> 1
        (else
         (error
          (string-append "Cannot convert to number "
                         (as-string x))))))


;; Convert x to a character. Integers, strings, booleans and symbols are supported.
;; If x is an integer between 0 and 255 return ASCII char number x. If x is a string return the first character in the string (which is supposed to be non-empty).
;; If x is a boolean return the character #\t for true and #\f for false. If x is a symbol return the first character of the print name of the string. Else return #\?.
(define (as-char x)
  (cond ((char? x) x)
        ((integer? x) 
           (if (and (>= x 0) (<= x 255)) 
               (integer->char x)
               #\?))
        ((string? x) (string-ref x 0))
        ((boolean? x) (if x #\t #\f))
        ((symbol? x)  (as-char (as-string x)))
        (else #\?)))

;; Convert x to a list. 
;; This function converts strings to a list of substring, which in the original string are separated by spaces, newlines, or tabs.
;; .internal-references "more general function" "string-to-list"
;; .example (as-list "xy z abc ") => ("xy" "z" "abc")
(define (as-list x)
  (cond ((string? x) (string-to-list x (list #\space (as-char 13) (as-char 10) #\tab)))
        ((list? x) x)
        ((pair? x) x)
        ((vector? x) (vector->list x)) 
        (else (list x))))

;; Convert a string to a list.
;; The second parameter is a list of separator characters.
(define (string-to-list str element-separator-chars)
 (filter (negate empty-string?)
  (string-to-list-help str "" '() element-separator-chars (string-length str))))

(define (string-to-list-help str next-el res-list element-separator-chars str-lgt)
 (if (= 0 str-lgt)
     (reverse (cons next-el res-list))     ; add last 'rest element: next-el
     (let ((next-char (string-ref str 0))
	   (rest-string (substring str 1 str-lgt)))
       (cond 
         ((memv next-char element-separator-chars) (string-to-list-help rest-string "" (cons next-el res-list) element-separator-chars (- str-lgt 1)))
         (else (string-to-list-help rest-string (string-append next-el (as-string next-char)) res-list element-separator-chars (- str-lgt 1)))))))

;; Convert x to a boolean. The strings "false", "no", and "NO" are converted to #f. Other strings are converted to #t.
(define (as-boolean x)
  (cond ((string? x) (if (or (equal? x "false") (equal? x "no") (equal? x "NO")) #f #t))
        ((boolean? x) x)
        (else (error "Cannot convert to boolean"))))

;; If x is considered true return #t else #f. 
;; See also as-boolean which is more versatile.
;; Recall that all values except #f, conveniently, act as a true value.
(define (turn-into-boolean x)
  (if x #t #f))

;; Convert x to C-style boolean values, 0 or 1.
;; Numbers are treated in the following way: If x is 0 the result is 0. If x is not 0 the result is 1.
;; Else 1 is returned if x is considered as true in Scheme, and 0 is returned if x is considered as false in Scheme.
;; .returns Either the integer 0 (for false) or the integer 1 (for true). 
(define (as-01-boolean x)
  (cond ((number? x)
          (if (= 0 x) 0 1))
        (else
          (if x 1 0))))

;; Return a string with the elements of str-lst separated by separator.
;; .parameter str-list A list of strings
;; .parameter separator A string which is used to separate the list elements in the resulting string.
(define (list-to-string str-lst separator)
  (string-merge 
     str-lst
     (make-list (- (length str-lst) 1) separator)))

;; Concatenate the strings in str-lst, and separate them by separator-str.
;; .parameter str-lst A list of strings
;; .parameter separator A string, a character or anything else that the function as-string can convert to a string.
;; .returns A string 
(define (string-append-with-separator str-lst separator)
  (letrec ((string-append-with-separator-1 
             (lambda (str-lst lgt-lst sep res)
                (cond ((= lgt-lst 0) res)
                      ((= lgt-lst 1) (string-append res (first str-lst)))
                      (else (string-append-with-separator-1 (cdr str-lst) (- lgt-lst 1) sep (string-append res (first str-lst) sep)))))))
    (string-append-with-separator-1 str-lst (length str-lst) (as-string separator) "")))


;; Return the decimal number n in base. 
;; .parameter n     A positive decimal integer.
;; .parameter base  The base of the number system. A possitive integer greater than 1.
;; .returns         A string which represents n in the number system with base.
;; .misc By coincidence equivalent to the native Scheme function number->string.
(define (number-in-base n base)
 (if (= n 0) "0"
  (let ((ciffer-list (reverse (ciffers-in-base n base))))
     (ciffer-output ciffer-list))))

(define (ciffers-in-base n base)
  (if (= n 0)
      '()
      (let ((rem (modulo n base))
            (newn (quotient n base)))
        (cons rem (ciffers-in-base newn base)))))

(define (ciffer-output ciffer-list)
 (apply string-append
    (map ciffer-translation ciffer-list)))

(define (ciffer-translation c)
  (cond ((<= c 9) (number->string c))
        ((and (> c 9) (< c 33)) (make-string 1 (integer->char (+ c 87))))
        (t "?")))


;;; String predicates.

; Is the string str empty
;(define (empty-string? str)
;  (= (string-length str) 0))

;; Is the string str empty
(define (empty-string? str)
  (string=? str ""))

;; A list of characters considered as blank space characters
(define white-space-char-list (list #\space (as-char 13) (as-char 10) #\tab))

;; Is the string str empty or blank. A blank string is composed of spaces, CRs, line feeds and tabs.
(define (blank-string? str)
  (or (empty-string? str) 
      (string-of-char-list? str white-space-char-list)))

;; Returns if the string str is numeric.
;; More specifically, does str consist exclusively of the ciffers 0 through 9.
;; A non-false value of the optional parameter signed? allows an initial '+' or '-' char as well.
;; .form (numeric-string? str [signed?])
(define (numeric-string? str . optional-parameters)
 (let ((signed? (optional-parameter 1 optional-parameters #f)))
   (if signed?
       (and (or (eqv? (string-ref str 0) #\+) (eqv? (string-ref str 0) #\-))
            (string-of-char-list? (substring str 1 (string-length str) ) (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 )))
       (string-of-char-list? str (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 )))))

;; Are all characters in str member of char-list (a list of characters).
(define (string-of-char-list? str char-list)
  (string-of-char-list-1? str char-list 0 (string-length str)))

(define (string-of-char-list-1? str char-list i lgt)
  (if (= i lgt)
      #t
      (and (memv (string-ref str i) char-list)
           (string-of-char-list-1? str char-list (+ i 1) lgt))))


;; Are all characters in str different from the characters in char list (a list of characters).
(define (string-of-negative-char-list? str char-list)
  (string-of-negative-char-list-1? str char-list 0 (string-length str)))

(define (string-of-negative-char-list-1? str char-list i lgt)
  (if (= i lgt)
      #t
      (and (not (memv (string-ref str i) char-list))
           (string-of-negative-char-list-1? str char-list (+ i 1) lgt))))

;; Does str contain sub-str as substring, starting at position pos?
;; An efficient implementation without any string copying, only character comparsion.
;; .parameter str The string to examine.
;; .parameter sub-str The string to look for in str.
;; .parameter pos The position where the match will have to occur (a non-negative integer).
;; .returns A boolean value.
(define (looking-at-substring? str pos sub-str)
  (looking-at-substring-1? str pos sub-str 0 (string-length str) (string-length sub-str)))

(define (looking-at-substring-1? str pos sub-str i lgt1 lgt2)
 (let ((a (+ i pos)))
  (cond ((= i lgt2) #t)
        ((and (< a lgt1) (< i lgt2) (eqv? (string-ref str a) (string-ref sub-str i)))
          (looking-at-substring-1? str pos sub-str (+ i 1) lgt1 lgt2))
        (else #f))))

;; Is t a substring of s? This function is almost identical to substring-index 
;; which in tells at which position (if any) t occurs in s.
;; .parameter s The string to examine.
;; .parameter t The string we are looking for as a substring of s.
;; .returns A boolean value.
;; .internal-references "related function" "substring-index"
(define (substring? s t)
  (let ((i (substring-index s 0 t)))
    (if i #t #f)))


;;; Other string functions.
;;; Among the functions in this section you will find string search and replacement functions. 


;; Return a list of two strings taken from str. ch is a character.
;; The first is the prefix of str up to the first occurence of ch
;; The second is the suffix from ch to the end of str
(define (split-on ch str)
 (let ((sp (split-point ch str)))
   (list (substring str 0 sp)

         (substring str (+ sp 1) (string-length str)))))

;; Return the character position where ch occurs the first time in str.
;; If it does not appear, the procedure returns #f.
;; This function allocates some temporary strings, and as such it is not efficient. 
;; Use find-in-string instead.
;; .internal-references "similar string find function" "substring-index"
(define (split-point ch str)
 (call-with-current-continuation
  (lambda (exit)
   (cond ((equal? str "") #f)
         ((eqv? ch (string-ref str 0)) 0)
         (else (let ((res (split-point ch (substring str 1 (string-length str)))))
                  (if (not res)
                      (exit #f)
                      (+ 1 res))))))))

;; Split the string str into a list of strings.
;; Consequtive portions of the strings, in which each character satisfies the char predicate pred, separate the elements of the resulting list.
(define (split-string-by-predicate str pred)
 (let ((p1 (find-in-string-by-predicate (negate pred) str 0)))
   (cond ((empty-string? str) '())
         ((not p1) '())
         (p1 (split-string-by-predicate-1 (substring str p1 (string-length str)) pred))
         (list str))))

(define (split-string-by-predicate-1 str pred)
 (let* ((strlen (string-length str))
        (p1 (find-in-string-by-predicate pred str 0))
        (p2 (find-in-string-by-predicate (negate pred) str p1)))
   (cond ((empty-string? str) '())
         ((and p1 p2)
            (cons (substring str 0 p1)
		  (split-string-by-predicate-1 (substring str p2 strlen) pred)))
         ((and p1 (not p2)) (list (substring str 0 p1)))
         ((and (not p1) (and (not p2))) (list str))
         (else '()))))


;; Search linearly for the character ch in the string str. 
;; An optional start postion start-post tells at which position to start the search (default is position 0).
;; Return the index of the first occurence of ch, or #f if it does not exist in str.
;; The index of the first character in a string is 0.
;; .internal-references "more general function" "find-in-string-by-predicate"
;; .form (find-in-string str ch [start-pos])
;; .parameter str The string in which to search.
;; .parameter ch The character we are searching for. 
;; .parameter start-pos The optional start-position of the search. Defaults to 0 (start of string). May also be boolean false.
(define (find-in-string str ch . optional-parameter-list)
 (let ((start-pos (optional-parameter 1 optional-parameter-list 0)))
  (if (and (boolean? start-pos) (not start-pos))
      #f
     (find-in-string-1 str ch start-pos (string-length str)))))

(define (find-in-string-1 str ch i lgt)
  (cond ((>= i lgt) #f)
        ((eqv? ch (string-ref str i)) i)
        (else (find-in-string-1 str ch (+ i 1) lgt))))

;; Search linearly for the character ch in the string str, beginning from the rear end of str.
;; Return the index of the last occurence of ch, or #f if it does not exist in str.
;; The index of the first character in a string is 0.
(define (find-in-string-from-end str ch)
 (let ((lgt (string-length str)))
  (find-in-string-from-end-1 str ch (- lgt 1) lgt)))

(define (find-in-string-from-end-1 str ch i lgt)
  (cond ((< i 0) #f)
        ((eqv? ch (string-ref str i)) i)
        (else (find-in-string-from-end-1 str ch (- i 1) lgt))))

;; Find the first character in str that satisfies the char-predicate pred, and return index of that char.
;; The search starts at position start-pos.
;; If start-pos is a boolean false, this function always returns boolean false.
;; This is a linear search in the string, corresponding to find-in-list for lists.
;; .internal-references "similar function" "find-in-list"
;; .form (find-in-string-by-predicate pred str [start-pos])
;; .parameter pred A string predicate function
;; .parameter str The string in which to search
;; .parameter start-pos The optional start-position of the search. Defaults to 0 (start of string). May also be boolean false.
(define (find-in-string-by-predicate pred str . optional-parameter-list)
 (let ((start-pos (optional-parameter 1 optional-parameter-list 0)))
  (find-in-string-by-predicate-1 pred str start-pos start-pos (string-length str))))

(define (find-in-string-by-predicate-1 pred str start-pos i lgt)
  (cond ((and (boolean? start-pos) (not start-pos)) #f)
        ((>= i lgt) #f)
        ((pred (string-ref str i)) i)
        (else (find-in-string-by-predicate-1 pred str start-pos (+ i 1) lgt))))

;; Find the last character in str that satisfies the char-predicate pred, and return index of that char.
;; The search starts at position start-pos.
;; If start-pos is a boolean false, this function always returns boolean false.
;; This is a rear end, linear search in the string, corresponding to find-in-list for lists.
;; .internal-references "similar function" "find-in-list-by-predicate"
;; .form (find-in-string-from-end-by-predicate pred str [start-pos])
;; .parameter pred A string predicate function
;; .parameter str The string in which to search
;; .parameter start-pos The optional start-position of the search. Defaults to the last position in str (end of string). May also be boolean false.
(define (find-in-string-from-end-by-predicate pred str . optional-parameter-list)
 (let* ((str-lgt (string-length str))
        (start-pos (optional-parameter 1 optional-parameter-list (- str-lgt 1))))
  (find-in-string-from-end-by-predicate-1 pred str start-pos start-pos str-lgt)))

(define (find-in-string-from-end-by-predicate-1 pred str start-pos i lgt)
  (cond ((and (boolean? start-pos) (not start-pos)) #f)
        ((< i 0) #f)
        ((pred (string-ref str i)) i)
        (else (find-in-string-from-end-by-predicate-1 pred str start-pos (- i 1) lgt))))



;; Starting from start-pos, skip characters in string from char-list.
;; Return the first index higher or equal to start-pos, which contains a character which is NOT in char-list.
;; This may be an index out of bound.
;; If start-pos is higher than the maximum legal string index, return start-post.
;; .parameter str The string on which this function works
;; .parameter char-list A list of characters
;; .parameter start-pos The position of the first character to consider. The index of the first character is 0.
;; .returns A string index, possibly out of bound (as described above). An integer.
(define (skip-chars-in-string str char-list start-pos)
  (skip-chars-in-string-1 str char-list start-pos (string-length str)))

(define (skip-chars-in-string-1 str char-list i lgt)
  (cond ((and (< i lgt) (memv (string-ref str i) char-list))
          (skip-chars-in-string-1 str char-list (+ i 1) lgt))
        ((and (< i lgt) (not (memv (string-ref str i) char-list)))
          i)
        (else i)))

;; Merge str-list-1 with str-list-2, returning one string.
;; Strings from the first list are merged with the strings from the second list.
;; In case one list is shorter than the other, the strings from the longests lists
;; are concatenated and appended
;; .example (string-merge (list "aa" "bb" "cc") (list "XX" "YY")) => "aaXXbbYYcc"
(define (string-merge str-list-1 str-list-2)
 (cond ((null? str-list-1) (apply string-append str-list-2))
       ((null? str-list-2) (apply string-append str-list-1))
       (else (string-append
              (car str-list-1) (car str-list-2)
              (string-merge (cdr str-list-1) (cdr str-list-2))))))

;; In in-string, substitute each occurence of character ch with the string str.
;; As a special case, str may be the empty string, in which case occurrences of the character ch is eliminated from str.
;; .parameter in-string A string
;; .parameter ch The character to be translated. A Scheme character
;; .parameter str The string to substitute occurrences of ch 
;; .returns A new string with the desired substitutions
(define (transliterate in-string ch str)
 (let ((str-factor (max (string-length str) 1)))
  (transliterate-1 in-string 0 (string-length in-string) 
                  (make-string (* (string-length in-string) str-factor) #\space) 0
                  ch str)))

(define (transliterate-1 in-string n in-length
         out-string m 
         ch str)
  ; n is the position in the input
  ; m is the positin in the output
  (cond ((= n in-length) (substring out-string 0 m))
        ((< n in-length)
           (let ((in-char (string-ref in-string n)))
              (if (eqv? in-char ch)
                  (begin (copy-string-into! out-string m str)
                         (transliterate-1 in-string (+ n 1) in-length out-string (+ m (string-length str)) ch str))
                  (begin (copy-string-into! out-string m (as-string in-char)) 
                         (transliterate-1 in-string (+ n 1) in-length out-string (+ m 1) ch str)))))
        (else (error "transliterate error")) ))

;; Take away all characters in the string str that satisfy pred. 
;; Returns a string shorter than or equal to the length of str.
;; This function is a non-destructive function.
;; .parameter str The string to be filtered.
;; .parameter pred A string predicate function
;; .returns The filtered string. All characters in string, in the same order, that do not satisfy pred.
;; .misc An iterative function
(define (filter-string pred str)
 (letrec ((filter-string-1
	   (lambda (pred str-lgt str i result j)
	     (cond ((>= i str-lgt) (substring result 0 j))
		   ((pred (string-ref str i)) (filter-string-1 pred str-lgt str (+ i 1) result j))
		   (else (begin
			   (string-set! result j (string-ref str i))
			   (filter-string-1 pred str-lgt str (+ i 1) result (+ j 1))))))))   
   (let* ((str-lgt (string-length str))
	  (result (make-string str-lgt)))
     (filter-string-1 pred str-lgt str 0 result 0))))


;; Delete the substring of length lgt from index i in the string str.
;; A non-destructive function which returns the result (a shorter string than the input).
;; i is supposed to be a valid index in str. If lgt is too long for str, we just delete to the end of str.
;; The first character is number 0.
(define (delete-string-portion str i lgt)
  (let* ((str-lgt (string-length str))
         (prefix (substring str 0 (max i 0)))
         (suffix (substring str (min (+ i lgt) str-lgt) str-lgt)))
    (string-append prefix suffix)))

; -----------------------------------------------------------------------------



;; In str1 replace all occurences of str2 with str3 and return the resulting string.
;; str2 is not allowed to be empty.
;; A non-destructive function which leaves all input strings unaffected.
(define (replace-string str1 str2 str3)
  (if (not (empty-string? str2))
      (replace-string-1 0 str1 str2 str3)
      (error (string-append "replace-string: Cannot replace empty string in " str1))))

; A helping function of replace-string which replaces from a given index i.
(define (replace-string-1 i str1 str2 str3)
  (let ((match-index (substring-index str1 i str2)))
    (if match-index 
        (replace-string-1
          (+ match-index (string-length str3))
          (put-into-string (delete-string-portion str1 match-index (string-length str2))  match-index str3)
          str2
          str3)
        str1)))


  

;; Put pre-putin at pre-index, and post-putit at post-index in the string str.
;; Return the result. Str is not affected.
;; .pre-condition pre-index is less than post-index.
(define (put-around-substring str pre-index pre-putin post-index post-putin)
 (put-into-string
    (put-into-string str post-index post-putin)
                         pre-index pre-putin))

;; Before the character with index put in putin-str into str, and return the resulting,
;; extended string. I.e, make room in the resulting string for putin-str, and slide a suffix of str 
;; to the right. Str is left unchanged. The first character is number 0.
(define (put-into-string str index putin-str)
 (let ((res (make-string (+ (string-length str) (string-length putin-str)))))
   (copy-string-into! res 0 (substring str 0 index))
   (copy-string-into! res index putin-str)
   (copy-string-into! res (+ index (string-length putin-str)) (substring str index (string-length str)))
   res))

;; Embed substring, as found in string, into embed-function.
;; A non-destructive function.
;; .parameter embed-function a function of one parameter, such as em, b.
;; .returns str with the first occurence of substring embedded into an activation of embed-function.
;; .example (embed-substring "LAML" "LAML is programmed in Scheme" em)
(define (embed-substring substring str embed-function)
  (let* ((i (substring-index str 0 substring)))
    (if i
        (let* ((pruned-str (delete-string-portion str i (string-length substring)))
	       (new-str (put-into-string pruned-str i (embed-function substring))))
          new-str)
        str)))

;; Copy source into target and overwrite a portion of target. 
;; Both target and source are strings, and i is an integer index.
;; The first char of source becomes chararter number i in the target string.
;; The first character in a string is number 0.
;; Target is mutated by this procedure.
;; If there is not enough room for source in target, only part of the source is copied into a suffix of target.
(define (copy-string-into! target i source)
  (copy-string-into-help! target i (string-length target) source 0 (string-length source)))

(define (copy-string-into-help! target i target-length source j source-length)
  ; A helping operation, doing the real work, of copy-string-into!
  (cond ((= i target-length) target)
        ((= j source-length) target)
        ((< j source-length) 
              (begin (string-set! target i (string-ref source j))
                     (copy-string-into-help! target (+ 1 i) target-length source (+ 1 j) source-length)))))

;; Return the index of the first occurence of find-str in str.
;; The search starts at str-index.
;; The first character in str has index 0.
;; If find-str is not a substring of str, starting the search at str-index, #f is returned.
(define (substring-index str str-index find-str)
 (let ((str-length (string-length str))
       (find-str-length (string-length find-str)))
  (cond ((= 0 (string-length find-str)) str-index)  ; new 10.9.98
        ((> str-index (- str-length find-str-length)) #f)
        ((substring-index-help str str-index str-length find-str 0 find-str-length) str-index)
        (else (substring-index str (+ 1 str-index) find-str)))))

(define (substring-index-help str str-index str-length find-str find-str-index find-str-length)
  ; return whether find-str matches at postion str-index at str.
  ; I.e., at boolean results from this function
  ; str-length is the length of str.
  ; find-str-length is the length of the remaining part of find-str to match.
  ; find-str-index is the actual index ind find-str.
  ; str-index is the actua index of str.
  (cond((= 0 find-str-length) #t)
       ((= str-index str-length) #f)
       ((eqv? (string-ref str str-index) (string-ref find-str find-str-index))
          (substring-index-help str (+ str-index 1) str-length find-str (+ 1 find-str-index) (- find-str-length 1)))
       (else #f)))


;; Return the first sentence in str (including a point).
;; The first sentence is running up to the first point followed by space or line termination.
(define (first-sentence-in-string str)
  (let* ((point-index (first-sentence-split-point str)))
    (if (number? point-index) (substring str 0 (+ 1 point-index)) str)))

;; Return all but the first sentence in str.
(define (but-first-sentence-of-string str)
  (let ((point-index (first-sentence-split-point str)))
    (if point-index (substring str (+ point-index 2) (string-length str)) "")))

; Return the split point of the first sentence in str.
; If no split point can be located, return #f.
(define (first-sentence-split-point str)
  (let* ((point-index-0 (substring-index str 0 ". "))
         (point-index-1 (substring-index str 0 (string-append "." (as-string (as-char 10)))))
         (point-index-2 (substring-index str 0 (string-append "." (as-string (as-char 13)))))
         (point-index-min (min-special point-index-0 point-index-1 point-index-2)))
    point-index-min))

(define (min-special . numbers-or-nulls)
  (min-special-1 numbers-or-nulls #f))

(define (min-special-1 numbers-or-nulls res)
  (cond ((null? numbers-or-nulls) res)
        ((boolean? res) (min-special-1 (cdr numbers-or-nulls) (car numbers-or-nulls)))
        ((and (number? res) (number? (car numbers-or-nulls)) (<  (car numbers-or-nulls) res))
            (min-special-1 (cdr numbers-or-nulls) (car numbers-or-nulls)))
        ((and (number? res) (number? (car numbers-or-nulls)) (>= (car numbers-or-nulls) res))
            (min-special-1 (cdr numbers-or-nulls) res))
        (else (min-special-1 (cdr numbers-or-nulls) res))))

;; Strip initial occurences of chars from char-list from string. Returns the empty string if given the empty string.
;; This function makes intermediate substrings, and as such it is not efficient.
(define (strip-initial-characters char-list string)
  (if (= (string-length string) 0)
      ""
      (if (memv (string-ref string 0) char-list)
          (strip-initial-characters char-list (substring string 1 (string-length string)))
          string)))

;; Strip trailing occurences of the characters in char-list from string. 
(define (strip-trailing-characters char-list string)
 (letrec ((last-non-char-list-index 
	   (lambda (i)
	     (cond ((< i 0) i)
		   ((memv (string-ref string i) char-list)
                      (last-non-char-list-index (- i 1)))
		   (else i)))) ; char i is not in char-list
         )
   (let ((i (last-non-char-list-index (- (string-length string) 1)))
        )
     (if (< i 0)
	 ""
       (substring string 0 (+ i 1))))))


;; Strip all initial space characters and lineshifting characters from string.
(define (strip-initial-spaces string)
  (strip-initial-characters 
     (list #\space (integer->char 10) (integer->char 13) (integer->char 9) (integer->char 12))
     string))
      

; con-par is in the html library file



(define quote-string (as-string #\"))

;; embed the string x in double string quotes
(define (string-it x)
  (string-append quote-string  x quote-string))

(define single-quote-string (as-string #\'))

;; embed the string x in single string quotes
(define (string-it-single x)
  (string-append single-quote-string x single-quote-string))

;; Exchange destructively char n and m in str. First character is number 0.
;; Not a function, thus no return value.
(define (exchange-chars-in-str! str n m)
  (let ((remember-char (string-ref str m)))
    (string-set! str m (string-ref str n))
    (string-set! str n remember-char)))

;; Ensure that the last character in str (a string) is ch (a char)
(define (ensure-final-character str ch)
  (let ((lgt (string-length str)))
    (if (and (> lgt 0)
             (eqv? ch (string-ref str (- lgt 1))))
         str
         (string-append str (as-string ch)))))

;; Repeat the string str n times.
;; If n equals 0, return the empty string. 
;; Causes a fatal error if n is negative.
(define (repeat-string str n)
  (cond ((< n 0) (error (string-append "repeat-string with negative repeat count is not supported: " (as-string n))))
        ((= n 0) "")
        (else (string-append str (repeat-string str (- n 1))))))

;; Unescape text with the escape character esc-char.
;; A pending escape character in text is just ignored.
;; Unescaping is the process of replacing a two-character text sequence ESC CHAR with CHAR.
;; .parameter text The input text string
;; .parameter esc-char The escape character. A Scheme char.
;; .example ab$c  -> abc
;; .example $.xy  -> .xy
;; .example $$xy  -> $xy
;; .example $$$$x -> $$x
;; .example xy$   -> xy
(define (unescape-text text esc-char)
 (let ((text-lgt (string-length text)))
  (unescape-1 text esc-char (make-string text-lgt) 0 0 text-lgt #f)))

; The procedure which does the real work of unescape-text.
; from-text is the original input text.
; esc-char is the escape character. 
; to-text is the resulting text, gradually mutated by this procedure.
; i is index in from-text and j is index in to-text
; from-text-length is the length of from-text.
; escape? is true if the next character in from-text is escaped. In that
; case, the next character will always appear in the to-text.
(define (unescape-1 from-text esc-char to-text i j from-text-lgt escape?)
 (cond ((= i from-text-lgt) (substring to-text 0 j))
       (escape?  ; previous char was escpae char.
          (string-set! to-text j (string-ref from-text i)) 
          (unescape-1 from-text esc-char to-text (+ i 1) (+ j 1) from-text-lgt #f))
       ((eqv? (string-ref from-text i) esc-char)
          (unescape-1 from-text esc-char to-text (+ i 1) j from-text-lgt #t))
       (else
          (string-set! to-text j (string-ref from-text i)) 
          (unescape-1 from-text esc-char to-text (+ i 1) (+ j 1) from-text-lgt #f))))

;; Rotate the string str n positions.
;; The first character of the resulting string will be (string-ref str n), and so on iteratively and cyclic.
(define (rotate-string str n)
  (let* ((lgt (string-length str))
         (n1 (remainder n lgt)))
    (string-append (substring str n1 lgt) (substring str 0 n1))))


;;; Functions that change letter case in string.
;;; Here comes a number of functions which changes the letter case of a string.
;;; In general we recommend use of the non-destructive versions of the functions, thus
;;; encouraging a clean, functional programming style. Due a difference between mutable and
;;; immutable strings, we have experienced problems with the destructive procedures in MzScheme.



;  Capitalizing characters and strings.

;; Mutate str to have an initial capital character. 
;; A destructive procedure. See capitalize-string-nd for a non-destructive variant.
;; .internal-references "non-destructive variant" "capitalize-string-nd"
(define (capitalize-string str)
  (string-set! str 0 (capitalize-char (string-ref str 0)))
  str)

;; Return str with capital, initial character. 
;; A non-destructive variant of capitalize-string.
;; .internal-references "destructive variant" "capitalize-string"
(define (capitalize-string-nd str)
  (let ((res (string-copy str)))
    (string-set! res 0 (capitalize-char (string-ref str 0)))
    res))

; if it makes sense, return the capital character corresponding to ch.
; else, just return ch
(define (capitalize-char ch)
  (let ((char-code (char->integer ch)))
    (if (lower-case-letter-code? char-code)
        (let ((offset (small-capital-offset char-code)))
          (integer->char (+ char-code offset)))
        ch)))

(define (lower-case-letter-code? n)
  (or (and (>= n 97) (<= n 122)) (= n 230) (= n 248) (= n 229)))

; in all cases, the distance between lower and upper case letters are -32 in the ASCII table
(define (small-capital-offset n)
 (cond ((and (>= n 97) (<= n 122)) -32)
       ((= n 230) -32)
       ((= n 248) -32)
       ((= n 229) -32)
       (else 0)))

; -----------------------------------------------------------------------------
; Upcasing all characters in a string:

;; Upcase all characters in str. This function is non-destructive, i.e., it does not change the parameter str.
(define (upcase-string str)
  (let ((res (make-string (string-length str) #\space)))
    (upcase-string-help! str res 0 (string-length str))))

(define (upcase-string-help! input output i lgt)
  (cond ((>= i lgt) output)
        (else (string-set! output i (capitalize-char (string-ref input i)))
              (upcase-string-help! input output (+ i 1) lgt))))

; -----------------------------------------------------------------------------

; Downcasing all characters in a string:

;; Downcase all characters in str. This function is non-destructive, i.e., it does not change the parameter str.
(define (downcase-string str)
  (let ((res (make-string (string-length str) #\space)))
    (downcase-string-help! str res 0 (string-length str))))

(define (downcase-string-help! input output i lgt)
  (cond ((>= i lgt) output)
        (else (string-set! output i (decapitalize-char (string-ref input i)))
              (downcase-string-help! input output (+ i 1) lgt))))

; -----------------------------------------------------------------------------

; decapitalizing characters and strings.

;; Mutate str to have lower case, initial character. 
;; A destructive procedure. See decapitalize-string-nd for a non-destructive variant.
;; .internal-references "non-destructive variant" "decapitalize-string-nd"
(define (decapitalize-string str)
  (string-set! str 0 (decapitalize-char (string-ref str 0)))
  str)

;; Return str with lower case, initial character. 
;; A non-destructive variant of decapitalize-string.
;; .internal-references "destructive variant" "decapitalize-string"
(define (decapitalize-string-nd str)
  (let ((res (string-copy str)))
    (string-set! res 0 (decapitalize-char (string-ref str 0)))
    res))

; If it makes sense, return the lower case character corresponding to ch.
; else, just return ch.
(define (decapitalize-char ch)
  (let ((char-code (char->integer ch)))
    (if (upper-case-letter-code? char-code)
        (let ((offset (large-capital-offset char-code)))
          (integer->char (+ char-code offset)))
        ch)))

(define (upper-case-letter-code? n)
  (or (and (>= n 65) (<= n 90)) (= n 198) (= n 216) (= n 197)))

(define (large-capital-offset n)
 ; in all cases, the distance between lower and upper case letters are -32 in the ASCII table
 (cond ((and (>= n 65) (<= n 90)) 32)
       ((= n 198) 32)
       ((= n 216) 32)
       ((= n 197) 32)
       (else 0)))

; ---------------------------------------------------------------------------------------------------

;;; Byte string functions.
;;; In this section we provide low-level functions that access binary data in strings. 
;;; This section has been added to LAML version 32.

;; Given a byte string - most significant byte first - return the decimal integer which it represents.
;; .internal-references "Inverse function" "int10-to-binary"
;; The inverse function is int10-to-binary.
;; .parameter byte-str A string of bytes.
;; .returns An integer number
;; .pre-condition byte-str is not empty
(define (byte-string-to-integer byte-str)
  (let* ((lgt (string-length byte-str)))
    (byte-string-to-integer-1 byte-str (- lgt 1) 0 1)))

(define (byte-string-to-integer-1 byte-str i res factor)
  (if (< i 0)
      res
      (byte-string-to-integer-1 byte-str (- i 1) (+ res (* (as-number (string-ref byte-str i)) factor)) (* factor 256))))

;; Convert a decimal integer n to a binary quantity, represented as string of length number-of-bytes.
;; If n is too large to be represented in number-of-bytes, an error occurs.
;; The inverse function is byte-string-to-integer.
;; .internal-references "Inverse function" "byte-string-to-integer"
;; .parameter n The integer to convert.
;; .parameter number-of-bytes  The desired number of bytes.
;; .result A string of bytes.
(define (int10-to-binary n number-of-bytes)
  (let* ((byte-list (binary-bytes-of-decimal-integer n))
         (lgt-byte-list (length byte-list)))
    (if (> lgt-byte-list number-of-bytes)
        (laml-error "int10-to-binary: Number does not fit in" number-of-bytes "byte(s): " n)
        (list->string (append
                         (make-list (- number-of-bytes lgt-byte-list) (as-char 0))    ; pad with initial zeros
                         byte-list)))))

(define (binary-bytes-of-decimal-integer n)
  (reverse (binary-bytes-of-decimal-integer-1 n)))

(define (binary-bytes-of-decimal-integer-1 n)
 (let ((rem (remainder n 256))
       (rest (quotient n 256)))
  (if (= rest 0)
      (list (as-char rem))
      (cons (as-char rem) (binary-bytes-of-decimal-integer-1 rest)))))
         
    
;; Make a character from two hex numbers
;; .parameter hx1 An integer number between 0 and 15
;; .parameter hx2 An integer number between 0 and 15
;; .returns A character
(define (make-char-2-hex hx1 hx2 )
  (as-char (+ (* hx1 16) hx2)))

;; Make a string, with single character, from two hex numbers. 
;; .parameter hx1 An decimal integer number between 0 and 15
;; .parameter hx2 An decimal integer number between 0 and 15
;; .returns A string of length one.
(define (make-byte-string-from-hex-2 hx1 hx2)
  (list->string (list (make-char-2-hex hx1 hx2))))

;; Make a string, with two characters, from four hex numbers. 
;; .parameter hx1 An decimal integer number between 0 and 15
;; .parameter hx2 An decimal integer number between 0 and 15
;; .parameter hx3 An decimal integer number between 0 and 15
;; .parameter hx4 An decimal integer number between 0 and 15
;; .returns A string of length two
(define (make-byte-string-from-hex-4 hx1 hx2 hx3 hx4)
  (list->string (list (make-char-2-hex hx1 hx2) (make-char-2-hex hx3 hx4) )))

;; Make a string, with three characters, from two six numbers.
;; .parameter hx1 An decimal integer number between 0 and 15
;; .parameter hx2 An decimal integer number between 0 and 15
;; .parameter hx3 An decimal integer number between 0 and 15
;; .parameter hx4 An decimal integer number between 0 and 15
;; .parameter hx5 An decimal integer number between 0 and 15
;; .parameter hx6 An decimal integer number between 0 and 15
;; .returns A string of length three
(define (make-byte-string-from-hex-6 hx1 hx2 hx3 hx4 hx5 hx6 )
  (list->string (list (make-char-2-hex hx1 hx2) (make-char-2-hex hx3 hx4) (make-char-2-hex hx5 hx6))))

;; Make a string, with four characters, from eight hex numbers.
;; .parameter hx1 An decimal integer number between 0 and 15
;; .parameter hx2 An decimal integer number between 0 and 15
;; .parameter hx3 An decimal integer number between 0 and 15
;; .parameter hx4 An decimal integer number between 0 and 15
;; .parameter hx5 An decimal integer number between 0 and 15
;; .parameter hx6 An decimal integer number between 0 and 15
;; .parameter hx7 An decimal integer number between 0 and 15
;; .parameter hx8 An decimal integer number between 0 and 15
;; .returns A string of length four.
(define (make-byte-string-from-hex-8 hx1 hx2 hx3 hx4 hx5 hx6 hx7 hx8)
  (list->string (list (make-char-2-hex hx1 hx2) (make-char-2-hex hx3 hx4) (make-char-2-hex hx5 hx6) (make-char-2-hex hx7 hx8))))


;; Given byte-string, which is binary data.
;; Return a non-binary string, of hex codes, space separated (for human readbility). 
;; Each byte gives rise to two hex codes.
;; The inverse function of hex-to-binary-string.
;; .parameter byte-string A string of bytes (binary data).
;; .returns An ASCII text string with grouped, human readable hexadecimal ciffers.
(define (binary-to-hex-string byte-string)
 (let* ((res (binary-to-hex-string-1 byte-string 0 (string-length byte-string)))
        (res-length (string-length res)))
   (if (> res-length 0) (substring res 0 (- res-length 1)) res)  ; removes a trailing space
 )
)

(define (binary-to-hex-string-1 byte-string i lgt)  
  (if (= i lgt)
      ""
      (let* ((byte (as-number (string-ref byte-string i)))
             (low  (remainder byte 16))
             (high (quotient byte 16)))
        (string-append 
          (upcase-string (number->string high 16)) (upcase-string (number->string low 16)) " "
          (binary-to-hex-string-1 byte-string (+ i 1) lgt))) 
  )
)

;; Given a human readable hex string, as produced by the sibling function called binary-to-hex-string.
;; Groups of two hex ciffers must be separated by one or more spaces or CRs.
;; Return the corresponding binary string.
;; The inverse function of binary-to-hex-string.
;; .parameter byte-string A string of bytes (binary data).
;; .returns An ASCII text string with grouped, human readable hexadecimal ciffers.
(define (hex-to-binary-string-relaxed hex-string)
  (if (= (string-length hex-string) 0)
      ""
      (let ((hex-string-extended (string-append hex-string " ")))
        (hex-to-binary-string-relaxed-1 hex-string-extended 0 (string-length hex-string-extended)))))


(define (hex-to-binary-string-relaxed-1 hex-string i lgt)
 (let ((j (find-in-string-by-predicate (lambda (c) (not (memv (as-number c) (list 9 10 13 32)))) hex-string i)))
  (if (or (= i lgt) (not j))
      ""
      (let* ((high-hex (as-string (string-ref hex-string j))) ; "1" .. "f"      
             (low-hex  (as-string (string-ref hex-string (+ j 1)))) ; "1" .. "f"
             (high-decimal (string->number high-hex 16))
             (low-decimal  (string->number low-hex 16) )
             )
        (string-append (as-string (as-char (+ (* 16 high-decimal) low-decimal))) 
                       (hex-to-binary-string-relaxed-1 hex-string (+ j 2) lgt))))))


;; Given a human readable hex string, as produced by the sibling function called binary-to-hex-string.
;; Groups of two hex ciffers must be separated by exactly one space.
;; Return the corresponding binary string.
;; The inverse function of binary-to-hex-string.
;; This function is like hex-to-binary-string relaxed, but with a stronger precondition.
;; .parameter byte-string A string of bytes (binary data).
;; .returns An ASCII text string with grouped, human readable hexadecimal ciffers.
(define (hex-to-binary-string hex-string)
  (if (= (string-length hex-string) 0)
      ""
      (let ((hex-string-extended (string-append hex-string " ")))
        (hex-to-binary-string-1 hex-string-extended 0 (string-length hex-string-extended)))))

(define (hex-to-binary-string-1 hex-string i lgt)
  (if (= i lgt)
      ""
      (let* ((high-hex (as-string (string-ref hex-string i))) ; "1" .. "f"      
             (low-hex  (as-string (string-ref hex-string (+ i 1)))) ; "1" .. "f"
             (high-decimal (string->number high-hex 16))
             (low-decimal  (string->number low-hex 16) )
             )
        (string-append (as-string (as-char (+ (* 16 high-decimal) low-decimal))) 
                       (hex-to-binary-string-1 hex-string (+ i 3) lgt)))))

; ---------------------------------------------------------------------------------------------------------------



;;; Message displaying and error handling procedures.
;;; Most message or error functions accept a list of messages which are string-converted and
;;; space separated before outputted.

; Aggreate the messages in list to a single message-string.
; Applies as-string before space separated concatenation.
(define (laml-aggregate-messages message-list)
 (string-merge 
  (map as-string message-list)
  (make-list (- (length message-list) 1) " ")))

;; Display a warning message line on standard output via the Scheme display function.
;; This is not a fatal error
(define (display-warning . messages)
  (display (string-append "Warning: " (laml-aggregate-messages messages))) (newline))

;; Display an error message - in terms of messages - and stop the program. 
;; This is a fatal event.
(define (display-error . messages)
  (error (laml-aggregate-messages messages)))

;; Display messages on standard output.
;; Not a warning, and not fatal by any means.
(define (display-message . messages)
  (begin (display (string-append (laml-aggregate-messages messages))) (newline)))


;; Stop the program with messages. 
;; This procedures takes an arbitrary number of parameters, which are string converted and string-appended
;; to the final error message.
(define (laml-error . messages)
  (error (laml-aggregate-messages messages)))


;; Return a list of error message strings for those conditions err-condition-message-list that are true.
;; The function returns #f in case all error conditions are false. 
;; err-condition-message-list is a property list (of even length) of error-condition error message pairs.
;; For each condition and message, this function checks the condition and returns the error message if the condition fails.
;; .parameter err-condition-message-list a property list of the form cond-1 mes-1 ... cond-n mes-n.
;; .returns A non-empty error message string, or #f.
(define (errors-among-conditions . err-condition-message-list)
  (errors-among-conditions-1 err-condition-message-list #f '()))

(define (errors-among-conditions-1 err-condition-message-list errors-found accumulated-error-messages)
  (cond ((null? err-condition-message-list) (if errors-found (reverse accumulated-error-messages) #f))
        (else (let ((error-condition (car err-condition-message-list))
                    (error-message (cadr err-condition-message-list)))
                (if error-condition 
                    (errors-among-conditions-1 (cddr err-condition-message-list) #t (cons error-message accumulated-error-messages))
                    (errors-among-conditions-1 (cddr err-condition-message-list) errors-found accumulated-error-messages))))
  ))




;;; File name, file path and URL functions.
;;; File paths are represented as strings in LAML. 
;;; As a convention, a non-empty relative file path always ends in a forward slash '/'.
;;; The empty string represents the empty relative file path.
;;; An absolute file path is recognized in both unix form (for instance "/x/y/") and Windows form (for instance "c:\\x\\").
;;; Internally in LAML, we work with unix representation of file paths (using forward slashes).


;; Return the filename component sans the final extension.
;; The extension, in a file name, is the part that follows the last `.'.
;; If no dot character is found the function returns file-name.
;; .misc This function does not work well if we use '.' as part of directory names.
(define (file-name-sans-extension file-name)
  (let ((extension-pos (find-in-string-from-end file-name #\.)))
    (if extension-pos
        (substring file-name 0 extension-pos)
        file-name)))

;; Return the part of file-name without extension and without an initial path.
;; Is also applicable on relative/absolute file path, and on URLs.
;; Works as expected even there are dots in the initial path.
(define (file-name-proper file-name)
 (let* ((extension-pos (find-in-string-from-end file-name #\.))
        (forward-slash-pos (find-in-string-from-end file-name #\/))
        (backward-slash-pos (find-in-string-from-end file-name #\\))
        (max-slash-pos (cond ((and forward-slash-pos backward-slash-pos) (max forward-slash-pos backward-slash-pos))
                             (forward-slash-pos forward-slash-pos)
                             (backward-slash-pos backward-slash-pos)
                             (else -1)))
        (extension-pos-1 (if (and extension-pos (> extension-pos max-slash-pos)) extension-pos #f))
      )
  (substring
      file-name
      (+ max-slash-pos 1)
      (if extension-pos-1 extension-pos-1 (string-length file-name)))))

;; Return the part of file-name, with a possible extension, but without an initial path.
;; Is also applicable on relative/absolute file path, and on URLs.
(define (file-name-proper-and-extension file-path)
  (let ((fnp (file-name-proper file-path))
        (fne (file-name-extension file-path)))
    (if (empty-string? fne)
        fnp
        (string-append fnp "." fne))))


;; Return the extension of file-name.
;; Is also applicable on relative/absolute file path, and on URLs.
;; If there is no extension, return the empty string.
;; The extension, in a file name, is the part that follows the last `.'.
;; This function handles dots in the initial path properly.
(define (file-name-extension file-name)
 (let ((extension-pos (find-in-string-from-end file-name #\.))
       (forward-slash-pos (find-in-string-from-end file-name #\/))
       (backward-slash-pos (find-in-string-from-end file-name #\\)))
  (cond ((and extension-pos forward-slash-pos (> extension-pos forward-slash-pos))
            (substring file-name (+ extension-pos 1) (string-length file-name)))
        ((and extension-pos forward-slash-pos (<= extension-pos forward-slash-pos))
            "")
        ((and extension-pos backward-slash-pos (> extension-pos backward-slash-pos))
             (substring file-name (+ extension-pos 1) (string-length file-name)))
        ((and extension-pos backward-slash-pos (<= extension-pos backward-slash-pos))
             "")
        (extension-pos (substring file-name (+ extension-pos 1) (string-length file-name)))
        (else ""))))


;; Return the initial path of file-path. 
;; The initial path of a file path is the prefix of the file path, without the proper file name
;; and without the extension. The initial path ends in a forward or backward slash, or it is empty.
;; Can also be applied on both absolute and relative file paths, and on absolute and relative URLs. 
(define (file-name-initial-path file-path)
 (let ((extension-pos (find-in-string-from-end file-path #\.))
       (forward-slash-pos (find-in-string-from-end file-path #\/))
       (backward-slash-pos (find-in-string-from-end file-path #\\)))
  (substring 
    file-path
    0
    (cond ((and forward-slash-pos backward-slash-pos) (+ 1 (max forward-slash-pos backward-slash-pos)))
	  (forward-slash-pos (+ 1 forward-slash-pos))
	  (backward-slash-pos (+ 1 backward-slash-pos))
	  (else 0))
  )))

;; Return whether x represents an absolute path to a file. Works on both Unix and Windows.
;; .parameter x A file path (a string)
(define (absolute-file-path? x)
  (let ((forward-slash-pos (find-in-string x #\/))
        (backward-slash-pos (find-in-string x #\\))
        (colon-pos (find-in-string x #\:)))
     (or (and (number? forward-slash-pos) (= 0 forward-slash-pos))
         (and (number? colon-pos) (= 1 colon-pos)
               (or
                 (and (number? backward-slash-pos) (= 2 backward-slash-pos))
                 (and (number? forward-slash-pos) (= 2 forward-slash-pos)))))))


;; Does the string x represent an absolute URL.
(define (absolute-url? x)
  (or (looking-at-substring? x 0 "http://")
      (looking-at-substring? x 0 "https://")
      (looking-at-substring? x 0 "file://")
      (looking-at-substring? x 0 "prospero://")
      (looking-at-substring? x 0 "wais://")
      (looking-at-substring? x 0 "telnet://")
      (looking-at-substring? x 0 "gopher://")
      (looking-at-substring? x 0 "news:")))

;; Does the string x represen a relative URL. 
;; .misc Experimental definition.
(define (relative-url? x)
  (and (string? x) (not (absolute-url? x)) (not (absolute-file-path? x))))

;; Return the name of the parent directory of dir (a string), or #f if dir is the root directory. Also return #f in case dir is the value #f.
(define (parent-directory dir)
 (if (and (boolean? dir) (not dir))
     #f
     (let* ((dir1 (ensure-final-character dir #\/))
	    (lgt (string-length dir1))
	    (dir2 (substring dir1 0 (max (- lgt 1) 0)))	; dir without ending slash
	    (forward-slash-pos (find-in-string-from-end dir2 #\/))
	    (backward-slash-pos (find-in-string-from-end dir2 #\\)))
       (cond ((and forward-slash-pos backward-slash-pos (>= forward-slash-pos backward-slash-pos))
	      (substring dir2 0 (+ 1 forward-slash-pos)))
	     ((and forward-slash-pos backward-slash-pos (>= backward-slash-pos forward-slash-pos))
	      (substring dir2 0 (+ 1 backward-slash-pos)))
	     (forward-slash-pos
	      (substring dir2 0 (+ 1 forward-slash-pos)))
	     (backward-slash-pos
	      (substring dir2 0 (+ 1 backward-slash-pos)))
	     (else #f)))))

;; Return the name of the leave directory of the directory dir.
;; In case dir is the absolute root, the value #f, or the empty directory string, this function returns #f.
;; .parameter dir A relative or absolute directory path (ends with '/').
;; .example (directory-leave-name "xxx/yyy/") => "yyy"
(define (directory-leave-name dir)
 (if (and (boolean? dir) (not dir))
     #f 
     (let* ((dir1 (ensure-final-character dir #\/))
	    (lgt (string-length dir1))
	    (dir2 (substring dir1 0 (max (- lgt 1) 0)))	; dir without ending slash
	    (res (file-name-proper dir2)))
       (if (or (empty-string? res) (eqv? #\: (string-ref dir2 (- (string-length dir2) 1))))
	   #f
	   res))))

;; Return the number of directory levels in between dir1 and dir2.
;; If dir1 is not a subdirectory of dir2, or dir2 is not a subdirectory of dir1 return #f.
;; .example (directory-level-difference "/x/x/z/v/" "/x/x/") = 2
;; .example (directory-level-difference "/x/x/" "/x/x/z/v/") = -2
(define (directory-level-difference dir1 dir2)
 (let ((dir1-lc (downcase-string dir1))
       (dir2-lc (downcase-string dir2)))
  (let ((res1 (directory-level-difference-1 dir1-lc dir2-lc 0))
        (res2 (directory-level-difference-1 dir2-lc dir1-lc 0)))
    (cond ((and res1 (number? res1)) res1)
          ((and res2 (number? res2)) (- res2))
          (else #f)))))

(define (directory-level-difference-1 dir1 dir2 n)
 (let ((parent-dir-1 (parent-directory dir1)))
   (cond ((and dir1 dir2 (equal? dir1 dir2)) n)
         ((and parent-dir-1 (string? parent-dir-1)) (directory-level-difference-1 parent-dir-1 dir2 (+ n 1)))
         ((not parent-dir-1) #f))))

;; Given a relative file path, return a list of path constituents.
;; .pre-condition dir is not an absolute file path.
;; This function supports both forward and backward slashes as separator between directory levels (both unix and windows conventions).
;; .example (relative-path-to-path-list "xxx/yyy/zzz/") = ("xxx" "yyy" "zzz")
;; .example (relative-path-to-path-list "xxx/yyy/zzz") = ("xxx" "yyy" "zzz")
;; .example (relative-path-to-path-list "xxx") = ("xxx")
(define (relative-path-to-path-list dir)
 (if (empty-string? dir)
     '()
     (let* ((dir1 (if (or (eqv? (string-ref dir (- (string-length dir) 1)) #\/) (eqv? (string-ref dir (- (string-length dir) 1)) #\\))
		      (substring dir 0 (- (string-length dir) 1))
		      dir))		; no trailing slash
	    (lgt (string-length dir1))
	    (forward-slash-pos (find-in-string dir1 #\/))
            (backward-slash-pos (find-in-string dir1 #\\))
            (slash-pos 
              (cond ((and forward-slash-pos backward-slash-pos)
                       (min forward-slash-pos backward-slash-pos))
                    (forward-slash-pos forward-slash-pos)
                    (backward-slash-pos backward-slash-pos)
                    (else #f)))
           )
       (if slash-pos
	   (cons (substring dir1 0 slash-pos)
		 (relative-path-to-path-list (substring dir1 (+ 1 slash-pos) lgt)))
	   (list dir1)))))

;; Return the relative path formed by the element of path-list.
;; The reverse function of relative-path-to-path-list.
(define (path-list-to-relative-path path-list)
  (ensure-final-character (list-to-string path-list "/")  #\/))

;; Ensure that the directory with path (string-append prefix-dir dir) exists.
;; If necessary, create dir in prefix-dir.
;; .pre-condition prefix-dir should be normalized (using for instance normalize-file-path) before calling this function.
;; .parameter prefix-dir An absolute directory path.
;; .parameter dir A single directory name
(define (ensure-directory-existence! prefix-dir dir)
  (if (not (directory-exists? (string-append prefix-dir dir)))
      (make-directory-in-directory prefix-dir dir)))

;; Ensure that the relative path, as represented by the parameter path, exists in prefix-dir.
;; This procedure creates the necessary directories in prefix-dir.
;; .pre-condition Both prefix-dir and path should be normalized (using for instance normalize-file-path) before calling this function.
;; .parameter prefix-dir An absolute directory path.
;; .parameter path A relative file path.
(define (ensure-directory-path-existence! prefix-dir path)
 (let ((path-list (relative-path-to-path-list path)))
   (ensure-directory-path-existence-1! prefix-dir path-list)))

(define (ensure-directory-path-existence-1! prefix-dir path-list)
  (if (not (null? path-list))
      (let ((first-path (car path-list)))
         (ensure-directory-existence! prefix-dir first-path)
         (ensure-directory-path-existence-1! (string-append prefix-dir first-path "/") (cdr path-list)))))

;; Ensure that the file f (proper name and extension) is non-existing in the directory d.
;; If not, add a numeric suffix to the proper name of f.
;; Return the possibly modified file name (proper name and extension).
(define (ensure-non-existing-file-in-dir f d)
 (if (not (file-exists? (string-append d f)))
     f
     (ensure-non-existing-file-in-dir-1 f d 1)))

(define (ensure-non-existing-file-in-dir-1 f d i)
 (let* ((pf (file-name-proper f))
        (ef (file-name-extension f))
        (nm (string-append pf "-" (as-string i) "." ef))
        (path (string-append d nm))
      )
 (if (not (file-exists? path))
     nm
     (ensure-non-existing-file-in-dir-1 f d (+ i 1)))))

;; Normalizes the abolute or relative file path by removal of redundant ".." levels.
;; .parameter path An absolute or relative file path.
;; .returns A normalized absolute or relative file path. Always slash terminated.
(define (normalize-file-path path)
  (cond ((absolute-file-path? path) (normalize-absolute-file-path path))
        (else (normalize-relative-file-path path))))

;; Normalizes the relative file path for redundant ".." levels.
;; Does always return a forward slash terminated relative path, or and empty path (the emtpy string).
;; Does never lead to a fatal error.
;; .parameter path A relative file path. May be empty (the empty string).
;; .returns The normalized relative path (a string).
(define (normalize-relative-file-path path)
  (let* ((path-list (relative-path-to-path-list path)))
     (normalize-relative-file-path-1 path-list '())))

; path-list is the relative path as a list.
; path-stack is a stack of already seen directories.
(define (normalize-relative-file-path-1 path-list path-stack)
  (cond ((null? path-list)                           
            (if (null? path-stack)
                ""
                (string-append (list-to-string (reverse path-stack) "/") "/")))
        ((and (equal? ".." (car path-list)) (not (null? path-stack)) (not (equal? ".." (car path-stack))))   
            (normalize-relative-file-path-1 (cdr path-list) (cdr path-stack)))                          
        ((and (equal? ".." (car path-list)) (not (null? path-stack)) (equal? ".." (car path-stack)))    
            (normalize-relative-file-path-1 (cdr path-list)  (cons ".." path-stack)))                   
        ((and (equal? ".." (car path-list)) (null? path-stack))                                         
            (normalize-relative-file-path-1 (cdr path-list) (cons ".." path-stack)))                    
        (else                                                                                           
            (normalize-relative-file-path-1 (cdr path-list) (cons (car path-list) path-stack)))         
  )
)

;; Normalizes the absolute file path for redundant ".." levels.
;; May result in a fatal error in cases where we try to exit through the root level via "..".
;; Returns a forward slash terminated absolute path.
;; .parameter path An absoulte file path.
;; .pre-condition path is an absolute file path.
;; .returns The normalized absolute path (a string).
(define (normalize-absolute-file-path abs-path)
  (let* ((prefix (prefix-part-of-absolute-path abs-path))
         (suffix (relative-part-of-absolute-path abs-path))
         (res (normalize-relative-file-path suffix)))
    (if (and (>= (string-length res) 2) (equal? ".." (substring res 0 2)))
        (laml-error "normalize-absolute-file-path: Not possible to normalize the absolute file path" abs-path)
        (string-append prefix res))))


;; Return the suffix part of the absolute file path  (the part following the initial "/" for instance).
;; .pre-condition abs-path is an absolute file path.
(define (relative-part-of-absolute-path abs-path)
  (let ((forward-slash-pos (find-in-string abs-path #\/))
        (backward-slash-pos (find-in-string abs-path #\\))
        (colon-pos (find-in-string abs-path #\:))
        (abs-path-length (string-length abs-path))
       )
    (cond ((and (number? forward-slash-pos) (= 0 forward-slash-pos))
             (substring abs-path 1 abs-path-length))
          ((and (number? colon-pos) (= 1 colon-pos)
                (or
                 (and (number? backward-slash-pos) (= 2 backward-slash-pos))
                 (and (number? forward-slash-pos) (= 2 forward-slash-pos))))
             (substring abs-path 3 abs-path-length))
          (else (laml-error "relative-part-of-absolute-path: The path" abs-path "is not an absolute file path.")))))


;; Return the prefix part of the absolute file path (the "/" or the "c:\\" for instance).
;; .pre-condition abs-path is an absolute file path.
(define (prefix-part-of-absolute-path abs-path)
  (let ((forward-slash-pos (find-in-string abs-path #\/))
        (backward-slash-pos (find-in-string abs-path #\\))
        (colon-pos (find-in-string abs-path #\:))
        (abs-path-length (string-length abs-path))
       )
    (cond ((and (number? forward-slash-pos) (= 0 forward-slash-pos))
             "/")
          ((and (number? colon-pos) (= 1 colon-pos)
                (or
                 (and (number? backward-slash-pos) (= 2 backward-slash-pos))
                 (and (number? forward-slash-pos) (= 2 forward-slash-pos))))
             (substring abs-path 0 3))
          (else (laml-error "prefix-part-of-absolute-path: The path" abs-path "is not an absolute file path.")))))

;; Return the complementary part of the absolute file path relative to (prefix-part-of-absolute-path abs-path).
;; Notice that (string-append (prefix-part-of-absolute-path abs-path) (but-prefix-part-of-absolute-path abs-path)) equals abs-path.
;; .pre-condition abs-path is an absolute file path.
;; .returns The largest possible relative file path taken from abs-path.
(define (but-prefix-part-of-absolute-path abs-path)
  (let ((forward-slash-pos (find-in-string abs-path #\/))
        (backward-slash-pos (find-in-string abs-path #\\))
        (colon-pos (find-in-string abs-path #\:))
        (abs-path-length (string-length abs-path))
        (abs-path-lgt (string-length abs-path))
       )
    (cond ((and (number? forward-slash-pos) (= 0 forward-slash-pos))
             (substring abs-path 1 abs-path-lgt))
          ((and (number? colon-pos) (= 1 colon-pos)
                (or
                 (and (number? backward-slash-pos) (= 2 backward-slash-pos))
                 (and (number? forward-slash-pos) (= 2 forward-slash-pos))))
             (substring abs-path 3 abs-path-lgt))
          (else (laml-error "but-prefix-part-of-absolute-path: The path" abs-path "is not an absolute file path.")))))
            

;; Return the inverse file path of path, as taken relative to dir.
;; Given dir as the current directory. If we follow path, and then follow the inverse return path (the result of this function) we are back in dir.
;; .parameter path A relative file path from dir
;; .parameter dir A directory, identified by an absolute file path
;; .returns A relative file path.
(define (inverse-return-path path dir)
 (if (empty-string? path)
     ""
     (let ((path-list (relative-path-to-path-list path))
           (leave-of-dir (directory-leave-name dir))
           (par-dir (parent-directory dir)))
       (path-list-to-relative-path (reverse (inverse-return-path-1 path-list leave-of-dir par-dir))))))

(define (inverse-return-path-1 path-list leave-dir par-dir)
  (cond ((null? path-list) '())
        ((equal? (car path-list) "..") (cons leave-dir (inverse-return-path-1 (cdr path-list) (directory-leave-name par-dir) (parent-directory par-dir))))
        (else  (cons ".." (inverse-return-path-1 (cdr path-list) (directory-leave-name par-dir) (parent-directory par-dir))))))


;;; Other functions. 
;;; Here follows a set of miscellaneous functions.


;; A quite special HTML line breaking function.
;; Html format str, either with br og p tags between lines.
;; depends on break-at-all from decoding stuff. 
;; Should perhaps be in the convenience library???
(define (re-break str)
 (letrec ((line-breaker (break-at-all #\newline)))  ; from decoding stuff
  (let* ((lines (line-breaker str))
         (line-lengths (map string-length lines))
         (max-line-length (max-int-list line-lengths)))
   (if (> max-line-length 120)
       (apply string-append 
          (map (lambda (ln) (string-append ln "<p>")) lines))
       (apply string-append 
          (map (lambda (ln) (string-append ln "<br>")) lines))))))

(define (max-int-list lst)
 (max-int-list-help lst 0))
 
(define (max-int-list-help lst res)
  (if (null? lst) 
      res
      (max-int-list-help (cdr lst) (max res (car lst)))))


;; Return a CR string
(define CR (as-string #\newline))

;; Return a CR string.
;; Please notice that there is a conflict between this function and the MzScheme url.ss net stuff.
;; (We should get rid of this function in LAML).
(define (newline-string)
  (as-string #\newline))


; Functions earlier in the cgi library:

;; Save the alist on a file named filename. Filename is assumed to be a full path to the file.
(define (save-a-list alist filename)
  (if (file-exists? filename)   ; new 31.3.2000
       (delete-file filename))
  (with-output-to-file filename
     (lambda () (write alist))))

;; Return a unique file name with prefix. The suffic becomes the current-time i seconds representation
(define (unique-timed-file-name prefix)
  (string-append prefix (number->string (current-time))))

;; Append x to file-name. The file is assumed to contain a Lisp list. x is added (actually pre-pended) to the list on the file,
;; and the file is written back. The ordering of the elements in the file list is not assumed to be important.
;; As a precondition, the file named file-name is assumed to exists.
(define (file-append file-name x)
 (let* ((port (open-input-file file-name))
        (contents (read port))
        (new-contents (append (list x) contents)))
  (close-input-port port)
  (delete-file file-name)  ; new!
  (let ((output-port (open-output-file file-name)))
    (write new-contents output-port)
    (close-output-port output-port))))

;; Read the first Lisp expression from file-name.
;; With an optional second parameter, read form number n from file.
;; .form (file-read file-name [n])
;; .pre-condition Assume that there are at least n forms on file
(define (file-read file-name . optional-parameter-list)
 (let ((n (optional-parameter 1 optional-parameter-list 1))
       (port (open-input-file file-name))
      )
   ; read n-1 forms:
   (for-each (lambda (n)  (read port)) (number-interval 1 (- n 1)))

   (let ((contents (read port)))
     (close-input-port port)
     contents)))

;; Read all Lisp expression from file-name.
;; This function returns these forms as a list of top level forms from the file.
(define (file-read-all file-name)
 (let* ((port (open-input-file file-name))
        (contents (file-read-all-1 port '())))
   (close-input-port port)
   (reverse contents)))

(define (file-read-all-1 port res)
 (let ((form (read port)))
   (if (eof-object? form)
       res
       (file-read-all-1 port (cons form res)))))

;; Write the list expression x to the file named file-name. 
;; The writing is done using the Scheme write function.
;; .parameter x An arbitrary value that can be written with write.
;; .parameter filename The name of the file (a string).
(define (file-write x file-name)
  (if (file-exists? file-name) (delete-file file-name))
  (let ((output-port (open-output-file file-name)))
    (write x output-port)
    (close-output-port output-port)))

;; Displays the first parameter x on a file named filename.
;; This is a minor convenience function, and an alternative to using the standard Scheme output functions.
;; .parameter x The string to be written.
;; .parameter filename The name of the file (a string).
(define (save-on-file x filename)
  (if (file-exists? filename) 
       (delete-file filename))
  (with-output-to-file filename
     (lambda () (display x))))


;; The identify function of one parameter
(define (id-1 x) x)




;; Is a (the first par) a multiplum of b (the last par)?
(define (multiplum-of a b)
  (= 0 (remainder a b)))



;; Copy the text file in from-path to the file in to-path. 
;; A quick and dirty solution by reading and writing strings to and from files.
;; If the destination file exists you must pass a third parameter, overwrite, with the value #t
(define (copy-text-file from-path to-path overwrite?)
  (if (and (file-exists? to-path) overwrite?) (delete-file to-path))
  (let ((contents (read-text-file from-path)))
    (if (not (file-exists? to-path))
        (write-text-file contents to-path)
        (error (string-append "copy-a-file: Overwriting an existing file requires a third overwrite #t parameter: " to-path)))))


;; Copy each of the files in the list files from source-dir to target-dir.
;; Both source-dir and target-dir ends in a slash.
;; If the optional boolean parameter warn-if-non-existing-source is #t a non-fatal warning is issued
;; if the source file does not exist. If the boolean parameter is #f, a fatal error will occur.
;; .form (copy-files files source-dir target-dir [warn-if-non-existing-source])
;; .parameter files A list of file names (without initial path).
;; .parameter source-dir The source directory in which the files are supposed to exist.
;; .parameter target-dir An existing directory to which the files are copied.
;; .parameter warn-if-non-existing-source A boolean parameter that controls the error reaction. Defaults to #f.
(define (copy-files files source-dir target-dir . optional-parameter-list)
 (let ((warn-if-non-existing-source (optional-parameter 1 optional-parameter-list #f)))
  (letrec ((copy-a-file 
            (lambda (f)
             (let ((target-file (string-append target-dir f))
                   (source-file (string-append source-dir f))
                  )
               (if (and (file-exists? target-file) (file-exists? source-file)) (delete-file target-file))
               (cond ((file-exists? source-file) (copy-file source-file target-file))
                     (warn-if-non-existing-source (display-warning (string-append "Could not copy the file " source-file)))
                     (else (laml-error "copy-file: Source does not exist:" source-file)))))))
   (for-each copy-a-file files))))


;; Ensure that the number x is in between min and max, or that min or max is returned.
;; More specifically, if x is not between min and max, the closest of min and max is returned.
;; .pre-condition min <= max
(define (min-max-limited x min max)
 (cond ((< x min) min)
        ((and (<= min x) (<= x max)) x)
        ((> x max) max)
        (else (laml-error "min-max-limited: Should not happen!" x min max))))
    
