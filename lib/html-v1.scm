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


;;;; The HTML version 1 library is an early attempt to create a more systematic bottom layer of LAML than
;;;; found in the HTML library. A much better solution has since been implemented in by the
;;;; <a href="../html4.0-loose/man/basis.html">basis mirror</a> and <a href="../html4.0-loose/man/surface.html">surface mirror</a> libraries.
;;;; The version number of this library (1) has nothing to do with the version of the HTML language.<p>
;;;;
;;;; Given a list of single tags and a list of double tags this library provides simple Scheme mirror functions.
;;;; In addition a number of helping functions
;;;; are contained in this library. Also a few page creation functions, built on top of the
;;;; automatically generated functions, are also provided.<p>
;;;;
;;;; The mirroring tool functions are not part of this library. They can be found in the tool 'The HTML mirroring tool'.<p>
;;;;
;;;; <b> This library is now obsolete</b>.  The direct counterpart, which is recommended, is  <a href="../html4.01-transitional-validating/man/basis.html">basis mirror</a>.
;;;; See also <a href = "../html4.01-transitional-validating/man/convenience.html"> the convenience library </a> and
;;;; the <a href="../html4.01-transitional-validating/man/surface.html">surface mirror</a> of HTML4.01 transitional.


; -----------------------------------------------------------------------------
;;; Tag generation. 
;;; Higher order functions which generate an html Scheme functions.

;; Given a tag-name generate an mirror function for the tag in Scheme. For double tags. The parameter tag-name is a symbol.
(define (generate-tag-function tag-name)
  (lambda (contents . attributes)
    (itag tag-name contents attributes)))

;; Given a tag-name generate an mirror function for the tag in Scheme. For single tags. The parameter tag-name is a symbol.
(define (generate-tag1-function tag-name)
  (lambda attributes
    (itag1 tag-name attributes)))


; --------------------------------------------------------------------------------------------------------------------------------
;;; General tag functions.
;;; A number of low level tag support functions.
;;; These functions are useful if we need to generate separate start and/or end tags.

(define (start-tag kind . attributes)
  ;; Return a HTML start tag.
  ;; kind is a string.
  ;; attributes is a lisp property list of key-symbol value pairs
  (if (null? attributes) 
      (string-append "<" kind ">")
      (let ((html-attributes (linearize-attributes attributes)))
         (string-append "<" kind " " html-attributes " >"))))

(define (end-tag kind)
  ;; Retun a html end tag
  (string-append "</" kind ">"))

(define (tag name contents . attributes)
 ;; Return a balanced html tag of both start and end tag:
 ;; <name attributes> contents </name>
 ;; name is a symbol or string.
 ;; contents is the stuff between the start and end tag: converted to string
 ;; attributes is a lisp property list of key-symbol value pairs
 (if (null? attributes)
  (string-append "<" (as-string name) ">"
                  (as-string contents)
                  "</" name ">")
  (let ((html-attributes (linearize-attributes attributes)))
   (string-append "<" (as-string name) " " html-attributes ">"
                  (as-string contents)
                  "</" name ">"))))

(define (itag name contents attributes)
 ;; Return a balanced html tag of both start and end tag, based on two parameters.
 ;; The 'i' in the function name means 'internal'.
 (if (null? attributes)
  (string-append "<" (as-string name) ">"
                  (as-string contents)
                  "</" name ">")
  (let ((html-attributes (linearize-attributes attributes)))
   (string-append "<" (as-string name) " " html-attributes ">"
                  (as-string contents)
                  "</" name ">"))))

(define (tag1 name . attributes)
  ;; Return a single html tag without end tag
  ;; <name attributes>
 (if (null? attributes)
     (string-append "<" (as-string name) ">")
     (let ((html-attributes (linearize-attributes attributes)))
       (string-append "<" (as-string name) " " html-attributes ">"))))


(define (itag1 name attributes)
 ;; Like tag1, but with a fixed number of paramers.
 ;; The 'i' in the function name means 'internal'.
 (if (null? attributes)
     (string-append "<" (as-string name) ">")
     (let ((html-attributes (linearize-attributes attributes)))
       (string-append "<" (as-string name) " " html-attributes ">"))))


; --------------------------------------------------------------------------------------------------------------------------------

;;; Attribute linearization. 

(define (linearize-attributes attr-list)
  ;; convert the Lisp property list attr-list to a string conforming
  ;; to the rules of an html attribute list
  (string-append  (linearize-attributes-1 (reverse attr-list) "" (length attr-list))))

(define (linearize-attributes-1 attr-list res-string lgt)
  (cond ((null? attr-list) res-string)
        ((>= lgt 2) (linearize-attributes-1 
                       (cddr attr-list)
                       (string-append (linearize-attribute-pair (car attr-list) (cadr attr-list)) " " res-string)
                       (- lgt 2)))
        ((< lgt 2) (error "Linearize-attributes-1: Called with an odd length attribute list. Not a Lisp property list"))))

(define (linearize-attribute-pair val attr)
  (string-append (as-string attr) " = " (string-it (as-string val))))


; -----------------------------------------------------------------------------
;;; Auto generated functions.
;;; Use the LAML tool html-tag-generation.scm in the tools directory to generate these functions.

; Generated by html-tag-generation.scm. Do not edit

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:a (generate-tag-function "a"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:address (generate-tag-function "address"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:applet (generate-tag-function "applet"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:b (generate-tag-function "b"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:big (generate-tag-function "big"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:blockquote (generate-tag-function "blockquote"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:body (generate-tag-function "body"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:caption (generate-tag-function "caption"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:center (generate-tag-function "center"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:cite (generate-tag-function "cite"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:code (generate-tag-function "code"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:comment (generate-tag-function "comment"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:dd (generate-tag-function "dd"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:dfn (generate-tag-function "dfn"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:dir (generate-tag-function "dir"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:div (generate-tag-function "div"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:dl (generate-tag-function "dl"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:dt (generate-tag-function "dt"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:em (generate-tag-function "em"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:embed (generate-tag-function "embed"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:font (generate-tag-function "font"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:form (generate-tag-function "form"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:frame (generate-tag-function "frame"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:frameset (generate-tag-function "frameset"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h1 (generate-tag-function "h1"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h2 (generate-tag-function "h2"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h3 (generate-tag-function "h3"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h4 (generate-tag-function "h4"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h5 (generate-tag-function "h5"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:h6 (generate-tag-function "h6"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:head (generate-tag-function "head"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:html (generate-tag-function "html"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:i (generate-tag-function "i"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:iframe (generate-tag-function "iframe"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:kbd (generate-tag-function "kbd"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:li (generate-tag-function "li"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:map (generate-tag-function "map"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:marquee (generate-tag-function "marquee"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:menu (generate-tag-function "menu"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:multicol (generate-tag-function "multicol"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:nobr (generate-tag-function "nobr"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:noframes (generate-tag-function "noframes"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:noscript (generate-tag-function "noscript"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:object (generate-tag-function "object"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:ol (generate-tag-function "ol"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:option (generate-tag-function "option"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:p (generate-tag-function "p"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:param (generate-tag-function "param"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:samp (generate-tag-function "samp"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:script (generate-tag-function "script"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:select (generate-tag-function "select"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:small (generate-tag-function "small"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:span (generate-tag-function "span"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:strike (generate-tag-function "strike"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:strong (generate-tag-function "strong"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:style (generate-tag-function "style"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:sub (generate-tag-function "sub"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:sup (generate-tag-function "sup"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:table (generate-tag-function "table"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:td (generate-tag-function "td"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:textarea (generate-tag-function "textarea"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:title (generate-tag-function "title"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:tr (generate-tag-function "tr"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:tt (generate-tag-function "tt"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:ul (generate-tag-function "ul"))

;; A Scheme mirrored HTML double tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag contents-form 'attr1 val1 'attr2 val2) </pre>
(define html:var (generate-tag-function "var"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:area (generate-tag1-function "area"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:base (generate-tag1-function "base"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:basefont (generate-tag1-function "basefont"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:bgsound (generate-tag1-function "bgsound"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:br (generate-tag1-function "br"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:col (generate-tag1-function "col"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:colgroup (generate-tag1-function "colgroup"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:hr (generate-tag1-function "hr"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:img (generate-tag1-function "img"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:input (generate-tag1-function "input"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:isindex (generate-tag1-function "isindex"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:link (generate-tag1-function "link"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:meta (generate-tag1-function "meta"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:nextid (generate-tag1-function "nextid"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:plaintext (generate-tag1-function "plaintext"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:pre (generate-tag1-function "pre"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:s (generate-tag1-function "s"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:spacer (generate-tag1-function "spacer"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:tbody (generate-tag1-function "tbody"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:tfoot (generate-tag1-function "tfoot"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:thead (generate-tag1-function "thead"))

;; A Scheme mirrored HTML single tag function. Auto generated by means of the functions in html-tag-generation.scm. <p>
;; Usage: <pre>(html:tag 'attr1 val1 'attr2 val2) </pre>
(define html:wbr (generate-tag1-function "wbr"))


; -----------------------------------------------------------------------------

;;; Higher level html-v1 functions. 
;;; Convenient HTML functions at a higher and more aggregate level, using html-v1 functions.
;;; Notice that some of these function assumes that other functions are defined.

(define (html:page title meta-contributions body   bg-color text-color link-color vlink-color)
 ;; Return an HTML page with html, head, meta and title tags.
 ;; Depends on the functions laml-standard-comment from the html library.
 ;; Furthermore, this function assumes that the functions tracing-comment and meta-tag-clauses are defined.
 ;; The function meta-tag-clauses must return a property list of meta-descriptors.
 ;; Tracing comment must return a string. Tracing-comment is meant to let us trace the LAML source files etc.
 ;; Meta-tag-clauses goes into a HTML meta-tag in the beginning of the generated document together with the parameter meta-contributions.
 ;; The html:page function is a more elaborate version of the page from from the html library.
 ;; The first part of the string output is the standard DOCTYPE.
 ;; The parameter meta-contributions is a list of meta-descriptors.
 ;; A meta-descriptor is a even length list of the form (key val key val ...).
 ;; The keys must be symbols, the values must be strings.
 ;; The last four parameters must consist of a background color, text color, link color and visited link color (all color RGB lists).
 ;; This function requires the plain html library to be loaded.

 (let ((meta-tags (append (meta-tag-clauses) meta-contributions)))
   (string-append
    (copyright-clause) (newline-string)
    (doctype-clause) (newline-string)
    (html:html
     (con
      (html:head
       (con
        (if (not (null? meta-tags)) (make-meta-contribution meta-tags) "")
        (html:title title)
       )
      )
      (html:body 
       body
       'bgcolor (apply rgb-string bg-color)
       'text (apply rgb-string text-color)
       'link (apply rgb-string link-color)
       'vlink (apply rgb-string vlink-color)
       )
      )
     ) 
    (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string)
    )))


(define (doctype-clause)
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")

(define (make-meta-contribution list-of-meta-descriptors)
  (apply
    string-append
    (map make-a-meta-contribution list-of-meta-descriptors)))

(define (make-a-meta-contribution meta-descriptor)
  (apply html:meta meta-descriptor))


(define (html:page-with-keypress-script title meta-contributions body   script-loading script keypress-action   bg-color text-color link-color vlink-color)
 ;; A version of html:page with Javascript scripts as a new fourth to sixth parameter. 
 ;; The parameter script-loading can either be static or dynamic (symbols). If static, included the script in the html document.
 ;; If dynamic, refer to the script in the a javascript subdirectory.
 ;; The parameter script is a list of two file path components (prefix-static-part js-filename), both strings.
 ;; If script-loading is static (string-append prefix-static-part js-filename) is the name of the script file, the contents of 
 ;; which must be included inline.
 ;; If script-loading is dynamic js-filename is the name of a file in the html/javascript directory, including extension.
 ;; The parameter keypress-action is supposed to be an activation of a Javascript function defined in script.
 ;; Uses the function read-text-file from the file-read library.

 (let ((meta-tags (append (meta-tag-clauses) meta-contributions))
       (script-clause (cond ((eq? script-loading 'static) (html:script (read-text-file (string-append (car script) (cadr script)))))
                            ((eq? script-loading 'dynamic) (html:script "" 'src (string-append "javascript/" (cadr script))))
                            (else (error "html:page-with-keypress-script in html-v1: problems determining type of script loading"))))
      )
   (string-append
    (doctype-clause) (newline-string)
    (html:html
     (con
      (html:head
       (con
        (if (not (null? meta-tags)) (make-meta-contribution meta-tags) "")
        (html:title title)
       )
      )
      script-clause
      (html:body 
       body
       'bgcolor (apply rgb-string bg-color)
       'text (apply rgb-string text-color)
       'link (apply rgb-string link-color)
       'vlink (apply rgb-string vlink-color)
       )
      )
     'onKeyPress keypress-action
     )
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string)
    )))


; Return a pre-page, without body and trailing html stuff.
; Used for imperative output of an HTML page.
; Usage: First call this function, the output the body, and then call html:post-page.
(define (html:pre-page-with-keypress-script title meta-contributions   script-loading script keypress-action   
                                                bg-color text-color link-color vlink-color)

 (let ((meta-tags (append (meta-tag-clauses) meta-contributions))
       (script-clause (cond ((eq? script-loading 'static) (html:script (read-text-file (string-append (car script) (cadr script)))))
                            ((eq? script-loading 'dynamic) (html:script "" 'src (string-append "javascript/" (cadr script))))
                            (else (error "html:page-with-keypress-script in html-v1: problems determining type of script loading"))))
      )
   (string-append
    (doctype-clause) (newline-string)

    (start-tag "html" 'onKeyPress keypress-action)
    (html:head
     (con
      (if (not (null? meta-tags)) (make-meta-contribution meta-tags) "")
      (html:title title)
     )
    )
    script-clause
    (start-tag "body"
               'bgcolor (apply rgb-string bg-color)
               'text (apply rgb-string text-color)
               'link (apply rgb-string link-color)
               'vlink (apply rgb-string vlink-color)))))


(define (html:post-page)
  (con 
    (end-tag "body")
    (end-tag "html") (newline-string)
    (laml-standard-comment) (newline-string)
    (tracing-comment) (newline-string)))



