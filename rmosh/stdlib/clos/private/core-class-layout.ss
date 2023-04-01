; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
; port to R6RS -- 2007 Christian Sloma
; 

(library (clos private core-class-layout)
 
  (export core-class-slot-names
          core-class-slot-count)

  (import (only (rnrs) define quote length))

  (define core-class-slot-names
    '( 
      
      ;;
      ;; initializer-fn : instance         -> unspecified 
      ;; getter-fn      : instance         -> value 
      ;; setter-fn      : instance * value -> unspecified
      ;;
      
      ;; list of direct super classes for this class
      direct-supers       ;; (class ...)
      
      ;; list of direct slots for this class
      direct-slots        ;; ((slot-name slot-option ...) ...)
      
      ;; list of this class and all super-classes
      ;; in the order in which they are searched for methods
      precedence-list     ;; (class ...)
      
      ;; list of slots (direct slots + slots of super-classes)
      slots               ;; ((slot-name slot-option ...) ...)
      
      ;; how many fields must be allocated for instances of this class
      number-of-fields    ;; integer
      
      ;; initializers for the fields
      field-initializers  ;; (initializer-fn ...)
      
      ;; getter and setter functions for the slots of this class
      ;; searchable by slot-name
      getters-and-setters ;; ((slot-name getter-fn setter-fn) ...)

      ;; name of the class
      definition-name     ;; symbol or #f
      
      )) ;; core-class-slot-names
  
  (define core-class-slot-count (length core-class-slot-names))

  ) ;; library (clos private core-class-layout)
