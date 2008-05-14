;=>man/color.sdoc

; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark.
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

; Experimentally documented with use of documentation-mark style, as a contrast
; to multi-semicolon style.


; !!!
; .title Reference Manual of the Color Library
; A library which contains the basic of handling colors in LAML.
;
; The library has encoding functions that convert rgb color lists, such as (rgb-color 255 255 255) 
; and (255 255 255) to a strings, such as "#ffffff". The primary color encoding function is rgb-color-encoding.
; The primary color representation function is make-rgb-color, which is accompanied by the color predicate rgb-color?
; a the color selectors red-of-rgb-color, green-of-rgb-color, and blue-of-rgb-color. <p>
;
; Of historical reasons we support two representation of colors. The first - the old representation - is just a list of red, green, 
; blue numbers (positive integers between 0 and 255), such as (255 0 255).
; The other is a tagged list of red, green, blue values such as (rgb-color 255 0 255), where rgb-color is the tag symbol. 
; Please be aware of the two different representations when you use this library.<p>
;
; The library also contains a set of color constants, all bound to the old color format (of backward compatibility reasons).


; !!
; Primary color encoding function.
; The function in this section, rgb-color-encoding, accepts a variety of different color formats as input.
; It returns a string of length seven, such as "#ff00ff". The output format is the primary color representation in most web contexts. 
; .section-id primary-function

;!Return a color encoding (a string of length seven such as "#123456") of color-pars.
; The color-pars parameter(s) to this function are very versatile.<p>
; If it is a color encoding string already, just return it.<br>
; If it is a color value which satisfies the predicate rgb-color?, return the encoding of this value. <br>
; If it is a symbol, return the color encoding bound to it. <br>
; If it is a string, transform it to a symbol and return the color encoding bound to it. <br>
; If it is a list like (list 1 2 3), consider it as a red-green-blue list, and return the color encoding of it. <br>
; If it is three individiual parameters, say r, g, and b, return the color encoding of red r, green g, and blue b. <p>
; If you care about efficiency, use can consider to use the function rgb-color instead of rgb-color-encoding.
; .old-example (rgb-color-encoding "red")
; .old-example (rgb-color-encoding 'red)
; .old-example (rgb-color-encoding "#ff0000")
; .old-example (rgb-color-encoding (make-color 255 0 0))
; .old-example (rgb-color-encoding '(255 0 0))
; .old-example (rgb-color-encoding (make-rgb-color 255 0 0))
; .old-example (rgb-color-encoding 255 0 0)
; .returns A string of length 7 of the format "#rrggbb".
; .internal-references "more efficient functions" "rgb-color" "rgb-color-list"
; .note This function works with both the new and old representation of colors in LAML.

(define (rgb-color-encoding . color-pars)
 (cond ((and (= 1 (length color-pars)) (rgb-color? (car color-pars)))
         (let ((the-color (car color-pars)))
           (rgb-color (red-of-rgb-color the-color) (green-of-rgb-color the-color) (blue-of-rgb-color the-color))))

       ((and (= 3 (length color-pars)) (number? (first color-pars)) (number? (second color-pars)) (number? (third color-pars)))
          (rgb-color (first color-pars) (second color-pars) (third color-pars)))

       ((and (= 1 (length color-pars)) (string? (car color-pars)) 
             (= 7 (string-length (car color-pars))) (eqv? #\# (string-ref (car color-pars) 0)))
          (car color-pars))

       ((and (= 1 (length color-pars)) (string? (car color-pars)) 
             (member (car color-pars) (list "red" "green" "blue" "white" "black" "yellow" "purple" "orange" "brown" "maroon" "grey" "silver" 
                                     "tetal" "aqua" "lime" "olive" "navy" "fuchsia")))
          (rgb-color-list (eval-cur-env (as-symbol (car color-pars)))))

       ((and (= 1 (length color-pars)) (symbol? (car color-pars)))
          (rgb-color-list (eval-cur-env (car color-pars))))

       ((and (= 1 (length color-pars)) (list? (car color-pars)) (= 3 (length (car color-pars))) 
             (number? (first (car color-pars))) (number? (second (car color-pars))) (number? (third (car color-pars))))  
          (rgb-color-list (car color-pars)))

       (else (laml-error "rgb-color: Cannot determine color" color-pars))))
      


;!! Secondary color encoding functions. 
; The functions in this section only work with the old version of the color representation.
; This is the untagged list representation, such as '(255 0 0). <p>
; For new development, the function
; make-rgb-color should be used together with the color encoding function rgb-color-encoding.

; Return an 'Internet list' encoding the color (list r g b). 
(define (rgb r g b)
  (list (number-in-base r 16) (number-in-base g 16) (number-in-base b 16)))

(define (pad-to-length2 str)
  (if (< (string-length str) 2)
      (string-append "0" str)
      str))


; Return an 'Internet color string" encoding the colors r, g, and b.
; .parameter r The amount of red - a decimal number between 0 and 255. 
; .parameter g The amount of green - a decimal number between 0 and 255. 
; .parameter b The amount of blue - a decimal number between 0 and 255. 
; .returns   A string of length 7 of the form "#rrggbb".
(define (rgb-string r g b)
  (let* ((lst3 (rgb r g b))
         (lst3-a (map pad-to-length2 lst3)))
    (apply string-append (cons "#" lst3-a))))

;! Return an 'Internet color string" encoding the colors r, g, and b.
; .form (rgb-color r g b)
; .parameter r The amount of red - a decimal number between 0 and 255. 
; .parameter g The amount of green - a decimal number between 0 and 255. 
; .parameter b The amount of blue - a decimal number between 0 and 255. 
; .returns   A string of length 7 of the form "#rrggbb".
(define rgb-color rgb-string)

; A variant of rgb-string, in which the colors are passed as a list of length 3.
; .parameter color-list A list of length 3. Each element of the list is a decimal integer between 0 and 255.
; .returns   A string of length 7 of the form "#rrggbb".
(define (rgb-string-list color-list)
  (rgb-string (car color-list) (cadr color-list) (caddr color-list)))

;!Returns the color encoding of (list r g b) given a list of three color numbers as parameter.
; .form (rgb-color-list color-triple-list)
; .parameter color-triple-list A list of length 3. Each element of the list is a decimal integer between 0 and 255.
; .returns   A string of length 7 of the form "#rrggbb".
(define rgb-color-list rgb-string-list)


; The hexidecimal ciffer, represented as a character, is translated to
; a number between 0 and 15. Both lower case and upper case letters between a and f (A and F)
; can be used to represent the high ciffers.
(define (hex-ciffer->decimal-ciffer x)
  (let ((n (char->integer x)))
    (cond ((and (>= n 48) (<= n 57)) (- n 48))
          ((and (>= n 97) (<= n 102)) (- n 87)) 
          ((and (>= n 65) (<= n 70)) (- n 55))
          (error (string-append "hex-ciffer->decimal-ciffer: The ciffer " (as-string x) " is not a hexadecimal ciffer")))))

;!! Color constructor, predicate, and selectors.
; The function make-rgb-color is the primary color constructor in LAML-based software.
; The predidate and the selectors only work with make-rgb-color.
; The function make-color is an old version of the constructor. 


;!Make and return a color represented by a red, green and blue constituent.
; This is the primary color constructor of LAML-based software.
; .parameter r The amount of red - a decimal number between 0 and 255. 
; .parameter g The amount of green - a decimal number between 0 and 255. 
; .parameter b The amount of blue - a decimal number between 0 and 255. 
; .returns A tagged list of color numbers.
; .internal-references "conversion function" "rgb-color-encoding"
; .old-example (make-rgb-color 255 0 0) => (rgb-color 255 0 0)
; .old-example (rgb-color-encoding (make-rgb-color 255 0 0)) => "#ff0000"
(define (make-rgb-color r g b)
  (list 'rgb-color r g b))

;!Is x a LAML color 
(define (rgb-color? x)
  (and (list? x) (= 4 (length x)) (eq? 'rgb-color (car x))))

;! Return the red constituent of the LAML color.
; .form (red-of-rgb-color color)
; .parameter color A color constructed with make-rgb-color.
; .internal-references "color constructor" "make-rgb-color"
(define red-of-rgb-color (make-selector-function 2 "red-of-rgb-color"))

;!Return the green constituent of the color.
; .form (green-of-rgb-color color)
; .parameter color A color constructed with make-rgb-color.
; .internal-references "color constructor" "make-rgb-color"
(define green-of-rgb-color (make-selector-function 3 "green-of-rgb-color"))

;!Return the blue constituent of the color.
; .form (blue-of-rgb-color color)
; .parameter color A color constructed with make-rgb-color.
; .internal-references "color constructor" "make-rgb-color"
(define blue-of-rgb-color (make-selector-function 4 "blue-of-rgb-color"))

;! Make and return the rgb-list representation of the color with r red, g green, and b blue.
; This is an old version of the color contructor.
; .misc Deprecated. Use make-rgb-color.
(define (make-color r g b)
  (list r g b))



;!! 
; Color constants.
; To stay backward compatible with a substantial amount of older LAML software, all color constants
; are bound to the old LAML color representation. Thus, for instance, the value of red is (255 0 0), and not
; (rgb-color 255 0 0). As an important observation, the primary color encoding function, rgb-color-encoding, accepts
; the value of the color constants as input (besides a number of other kinds of input).

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define red (make-color 255 0 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define dark-red (make-color 210 0 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define green (make-color 0 255 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define green1 (make-color 202 240 179))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define green2 (make-color 182 248 197))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define blue (make-color 0 0 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define white (make-color 255 255 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define black (make-color 0 0 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define yellow (make-color 255 255 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define purple (make-color 255 0 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define light-blue (make-color 0 255 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define blue1 (make-color 170 241 249))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define blue2 (make-color 204 255 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define blue3 (make-color 198 203 253))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define orange (make-color 211 90 18))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define dark-yellow (make-color 228 211 5))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define grey1 (make-color 145 145 145))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define grey2 (make-color 210 210 210))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define brown (make-color 166 71 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define maroon   (make-color 128 0 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define grey     (make-color 128 128 128))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define silver   (make-color 192 192 192))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define tetal    (make-color 0 128 128))

;! A color constant. A color is represented as a list of integers of length three (rgb).

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define aqua     (make-color 0 255 255))

;! A color constant. A color is represented as a list of integers of length three (rgb).

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define lime     (make-color 0 255 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define olive    (make-color 128 128 0))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define navy     (make-color 0 0 128))

;! A color constant. A color is represented as a list of integers of length three (rgb).
(define fuchsia  (make-color 255 0 255))

