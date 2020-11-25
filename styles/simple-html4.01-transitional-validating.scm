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


;;;; This is the simple LAML style based on the validating html4.01-transitional libraries.<p>
;;;; The primary pupose of using this style is to load a number of central LAML libraries
;;;; (time, color, file-read, and of course the HTML4.01 mirror libraries basic, surface and convenience).<p>
;;;; Thus, the use of this document style makes life a little easier for you, compared to a manual loading
;;;; of a number of the LAML libraries.<p>
;;;; Usage: <kbd>(laml-style "simple-html4.01-transitional-validating")</kbd><p>
;;;; <b>We recommend that you use one of the XHTML mirrors in LAML instead of HTML4.01.</b><p>


; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "html4.01-transitional-validating/basis.scm")
(lib-load "html4.01-transitional-validating/surface.scm")
(lib-load "html4.01-transitional-validating/convenience.scm")
(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

(define cgi-testing #f)

;  Return a list of two strings (date time)
(define (date-time-1 second-count)
 (let ((time-list (time-decode second-count)))
  (let* ( (year (modulo (first time-list) 100))
          (month (second time-list))
          (day (third time-list))
          (hours (fourth time-list))
          (minutes (fifth time-list))
          (seconds (sixth time-list)))
    (list 
      (string-append (number->string day) "."
                     (number->string month) "."
                     (number->string year))
      (string-append (zero-pad-string (number->string  hours)) "."
                     (zero-pad-string (number->string  minutes))
                     )))))

(define (last-update)
 (font-1 1 red (when-generated)))

;; A list of four colors: the background, the text, the link, and the visited link colors.
(define default-bg-text-link-vlink-colors (list white black blue blue))

;; Make a generic WWW page.
;; Title and body are the title and body of the page.
;; In the body you can use all the functions from the LAML html4.0 libraries.
;; Colors is an optional list of four parameters.
;; If given, it must be a list of exactly four colors: 
;; Background color, text color (foreground color), link color, and visited link color. 
;; A color can be made by the function make-color.
;; .reference "similar procedure" "write-html" "../../man/laml.html#write-html"
;; .misc We do not recommend this procedure anymore. Instead, insert the generic page template via the Emacs Laml menu.
(define (generic-page title body . colors)
 (let* ((colors1 (cond ((and colors (= (length colors) 4)) colors)
                       ((null? colors) default-bg-text-link-vlink-colors) ; default
                       (else (error "generic-page: color list can be empty or of length four: bg, text, link, vlink"))))
        (bg-color (first colors1))
        (text-color (second colors1))
        (link-color (third colors1))
        (vlink-color (fourth colors1))
        (destination-directory (if (startup-directory) (startup-directory) ""))
        (laml-source-file-name-without-extension (source-filename-without-extension))
       )
  (write-text-file
    (page-1 title '()
          (con body (vertical-space 2) (last-update))
          bg-color text-color link-color vlink-color)
    (string-append destination-directory laml-source-file-name-without-extension ".html"))))

(define (meta-tag-clauses)
   (list
     (list 'name "Generator" 'content "LAML")
;     (list 'http-equiv "Content-Type" 'content "text/html; charset=utf-8")
   )
)

;; A more user friendly version of generic-page, in which the title also is used as header of the page.
;; .reference "similar procedure" "write-html" "../../man/laml.html#write-html"
;; .misc We do not recommend this procedure anymore. Instead, insert the generic page template via the Emacs Laml menu.
(define (generic-page-1 title body . colors)
  (let ((colors-1 (if (not (null? colors)) colors default-bg-text-link-vlink-colors)))
   (generic-page
     title
     (con
       (h 1 title)
       body)
     (first colors-1) (second colors-1) (third colors-1) (fourth colors-1))))


(laml-welcome)






