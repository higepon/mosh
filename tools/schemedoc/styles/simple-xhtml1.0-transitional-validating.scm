; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2002  Kurt Normark.
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

;;;; This is the XML-in-LAML mirror of XHTML1.0-transitional.<p>
;;;;
;;;; The <a href="../../lib/xml-in-laml/mirrors/man/xhtml10-transitional-mirror.html">XHTML 1.0 mirror functions</a>
;;;; and the <a href= "../../lib/xml-in-laml/man/xml-in-laml.html"> generic XML-in-LAML library </a> are both important for this style.<p>
;;;; 
;;;; The use of this library takes care of loading a number of useful libraries
;;;; (time, color, file-read, and of course the XHTML1.0 mirror libraries).
;;;; The use of this document style makes life a little easier for you, compared to a manual loading
;;;; of the mentioned LAML libraries.<p>
;;;;
;;;; Usage: <kbd>(laml-style "simple-xhtml1.0-transitional-validating")</kbd><p>
;;;; There are no external definitions in this library.
;;;; .title Manual of the LAML style for the XHTML1.0 transitional, validating mirror


(lib-load "xml-in-laml/xml-in-laml.scm")
(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")


(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")


(laml-welcome)






