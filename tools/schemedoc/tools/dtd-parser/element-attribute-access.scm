;;;; A library of function which provides access to various aspects of parsed
;;;; elements and attributes.
;;;; Assumes that all elements (on parsed form) are available in the global variable element-list.
;;;; Similarly, assume that all attributes (on parsed form) are available in the global variable attribute-list.

; Attribute and element selectors - ad hoc
(define element-name-of (make-selector-function 2 "element-name-of"))
(define attribute-name-of (make-selector-function 2 "attribute-name-of"))
(define attribute-list-of (make-selector-function 3 "attribute-list-of"))
(define rhs-of-element (make-selector-function 5 "rhs-of-element"))

; Return the list of all element names in element-list
(define (all-element-names element-list)
  (map element-name-of element-list))

; Return the element (list representation) of el-name (a string or symbol) in element-list.
; If element does not exist, return #f.
(define (find-element-in-parsed-dtd el-name element-list)
 (let* ((el-name1 (as-string el-name))
        (candidate-element 
         (find-in-list 
          (lambda (el)
            (equal? (element-name-of el) el-name1))
         element-list))
       )
   (if candidate-element 
       candidate-element
       #f))) ;  (laml-error "find-element-in-parsed-dtd: Cannot find element" el-name1 "in the element list.") 

; Return the attribute (list representation) of el-name (a string or symbol) in attribute-list.
; Return #f if no attributes of el-name can be located in attribute-list
(define (find-attribute-in-parsed-dtd el-name attribute-list)
 (let* ((el-name1 (as-string el-name))
        (candidate-attribute
         (find-in-list 
          (lambda (el)
            (equal? (attribute-name-of el) el-name1))
         attribute-list))
       )
   (if candidate-attribute
       candidate-attribute
       #f)))   ; (laml-error "find-attribute-in-parsed-dtd: Cannot find attribute descriptor of element" el-name1 "in the element list.")

;; Return the list of sub element names given element-name.
;; Element-name is a string, and the returned list is a list of strings (without duplicates).
(define (sub-element-names-of-element-name element-name)
 (let ((element (find-element-in-parsed-dtd element-name element-list)))
   (if element
       (sub-element-names-of-element element)
       (begin
         (display-warning "Non-existing parsed element in element-list:" element-name)
         '()))))

;; The reflexive, transitive closure of sub-element-names-of-element-name.
;; Thus, this function returns all the names of the elements that can appear
;; as sub elements (sons, sons of sons etc) from element-name.
(define (sub-element-names-of-element-name* element-name)
 (sub-element-names-of-element-name*-help (list element-name) '() '()))

;; Return the list of possible attribute names of element-name (a string or symbol).'
;; The result is a list of strings.
(define (attributes-of-element-name element-name)
 (let ((attribute-structure (find-attribute-in-parsed-dtd element-name attribute-list)))
   (if attribute-structure
       (let ((attribute-descr-list (third attribute-structure)))
          (map car attribute-descr-list))
       (begin
         (display-warning "Non-existing parsed attribute of element in attribute-list:" element-name)
         '()))))

;; Return the possible list of all attribute names of element-name together with
;; the attribute names of all direct and indirect sub-elements of element-name.
;; Thus, this function returns all possible attributes in a tree rooted by element-name.
(define (attributes-of-element-name* element-name)   
 (let ((sub-element-names (sub-element-names-of-element-name* element-name)))
   (remove-duplicates-by-predicate (flatten (map attributes-of-element-name sub-element-names)) equal?)))

; The function doing the real work of sub-element-names-of-element-name*
(define (sub-element-names-of-element-name*-help element-names result tried)
   (cond ((null? element-names) result)
         ((member (car element-names) tried)
           (sub-element-names-of-element-name*-help (cdr element-names) result tried))
         (else 
           (let* ((element-name (car element-names))
                  (direct-contributions (sub-element-names-of-element-name element-name)))
             (sub-element-names-of-element-name*-help 
                 (append (cdr element-names) direct-contributions)
                 (remove-duplicates-by-predicate (append result direct-contributions (list element-name)) equal?)
                 (cons element-name tried))))))
  

; Return the names of the sub-elements of e (a list of strings).
; e is an XML element, as represented in the parsed Lisp representation.
(define (sub-element-names-of-element e)
  (let ((rhs (rhs-of-element e)))
   (cond ((and (list? rhs) (eq? (car rhs) 'element-content))
            (remove-duplicates-by-predicate (sub-element-names-of-element-element-content e rhs) equal?))
         ((and (list? rhs) (eq? (car rhs) 'mixed-content))
            (remove-duplicates-by-predicate (sub-element-names-of-element-mixed-content e rhs) equal?))
         ((and (symbol? rhs) (eq? rhs 'empty))
            '())
         ((and (symbol? rhs) (eq? rhs 'any))
           (all-element-names element-list))  ; element-list is a global variable from tools/xml-in-laml.scm
         (else (laml-error "sub-element-names-of-element: Should not happen")))))

(define (sub-element-names-of-element-mixed-content e rhs)
  (cond ((and (= 2 (length rhs)) (eq? (cadr rhs) 'pcdata))
           '())
        ((and (= 2 (length rhs)) (list? (cadr rhs)))
          (let ((second (cadr rhs)))
            (if (and (>= (length second) 2) (eq? (car second) 'choice) (eq? (cadr second) 'pcdata))
                (cddr second)
                (laml-error "sub-element-names-of-element-mixed-content: malformed mixed contents" rhs))))
        (else (laml-error "sub-element-names-of-element-mixed-content: Should not happen"))))

(define (sub-element-names-of-element-element-content e rhs)
 (let* ((substance (cadr rhs)))
   (sub-element-names-of-element-element-content-1 substance)))

(define (sub-element-names-of-element-element-content-1 element-content)
 (let ((kind (car element-content)))
   (cond ((eq? kind 'name) (list (third element-content)))
         ((eq? kind 'seq) 
            (flatten (map sub-element-names-of-element-element-content-1 (cddr element-content))))
         ((eq? kind 'choice) 
            (flatten (map sub-element-names-of-element-element-content-1 (cddr element-content))))
         ((eq? kind 'empty) '())
         (else (laml-error "sub-element-names-of-element-element-content-1: Unknown kind of elment content:" kind)))))

; ---------------------------------------------------------------------------------------------------

; Return the names of elements in parsed-dtd-element-list which refers directly to el-name.
; .returns A list of element names (a list of strings).
(define (all-elements-using el-name parsed-dtd-element-list)
 (map element-name-of
   (filter (lambda (el-structure) (member el-name (sub-element-names-of-element el-structure)))
           parsed-dtd-element-list)))

