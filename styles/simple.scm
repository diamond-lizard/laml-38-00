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


;;;; This is the simple LAML style.
;;;; Using this style, a number of useful LAML libraries will be loaded.
;;;; In addition, the simple style defines the function generic-page (in two variants) which
;;;; is useful to make a simple WWW page. <p>
;;;;
;;;; <b> This style is obsolete now</b>. Use <a href = "simple-html4.01-transitional-validating.html"> simple 4.01 validating transitional</a> instead,
;;;; or even better one of the XHTML mirrors
;;;; which are validating mirrors of HTML in Scheme.

(define laml-source-file-name-without-extension  (source-filename-without-extension))

(define destination-directory 
  (let ((source-dir (startup-directory scheme-system)))
    (if source-dir source-dir "")))

;; Define the name of the generated html file.
;; Using the Emacs activation of SCM, the file name is taken from the buffer name, and as a consequence
;; it is not necessary to use this function.
;; Using other means of activation, this function can be used to set the name of the generated html file.
;; File is without extension.
(define (set-file-name! file)
  (set! laml-source-file-name-without-extension file))

(define target-extension "html")

(define cgi-testing #f)

; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "html.scm")
(lib-load "html-v1.scm")
(lib-load "hex.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

;  Return a list of two strings (date time)
(define date-time-1 date-time)

(define (last-update)
  (let* ((ct (current-time))
	 (d-t (date-time-1 ct))
	 (date (car d-t))
	 (time (cadr d-t)))
    (font 1 red (con "Generated: " date ", " time))))

;; A list of four colors: the background, the text, the link, and the visited link colors.
(define default-bg-text-link-vlink-colors (list white black blue blue))

;; Make a generic WWW page.
;; Title and body are the title and body of the page.
;; In the body you can use all the useful functions from the LAML html libraries.
;; Colors is an optional parameter.
;; If given, it must be a list of exactly four colors: Background color, text color (foreground color), link color, and visited link color. 
;; A color can be made by the function make-color.
(define (generic-page title body . colors)
 (let* ((colors1 (cond ((and colors (= (length colors) 4)) colors)
                       ((null? colors) default-bg-text-link-vlink-colors) ; default
                       (else (error "generic-page: color list can be empty or of length four: bg, text, link, vlink"))))
        (bg-color (first colors1))
        (text-color (second colors1))
        (link-color (third colors1))
        (vlink-color (fourth colors1)))
  (write-text-file
    (page title
          (con body (vertical-space 2) (last-update))
          bg-color text-color link-color vlink-color)
    (string-append destination-directory laml-source-file-name-without-extension ".html"))))

;; A more user friendly version of generic-page, in which the title also is used as header of the page.
(define (generic-page-1 title body . colors)
  (let ((colors-1 (if (not (null? colors)) colors default-bg-text-link-vlink-colors)))
  (generic-page
     title
     (con
       (h 1 title)
       body)
     (first colors-1) (second colors-1) (third colors-1) (fourth colors-1))))
    

(laml-welcome)



