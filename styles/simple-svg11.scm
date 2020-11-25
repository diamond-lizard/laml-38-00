; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2005  Kurt Normark.
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



;;;; This is the XML-in-LAML mirror of SVG1.1.<p>
;;;; As such, this style relies on a SVG dependent part which is derived from the SVG1.1 DTD, and
;;;; on a language independent part in lib/xml-in-laml/xml-in-laml.scm.<p>
;;;; The only purpose of using this style is to load a number of  LAML libraries
;;;; (such as time, color, file-read, and of course the SVG1.1 mirror library).<p>
;;;; The use of this document style makes life a little easier for you, compared to a manual loading
;;;; of a number of the LAML libraries.<p>
;;;; Usage: <kbd>(laml-style "simple-svg11")</kbd><p>
;;;; There are no external definitions in this library.
;;;; .title Manual of the Simple LAML style for SVG1.1 - validating

; general.scm is loaded by laml.scm

; In order to deal with case sensitive attribute names in SVG.
; From version 26: Case sensitivenes is now the general rule. But we keep it here anyway.
(if (or (eq? scheme-system 'mzscheme) (eq? scheme-system 'mzscheme-200))
    (read-case-sensitive #t))


(lib-load "xml-in-laml/xml-in-laml.scm")
(lib-load "xml-in-laml/mirrors/svg11-mirror.scm")

(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")


(laml-welcome)






