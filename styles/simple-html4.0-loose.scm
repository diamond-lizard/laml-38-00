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


;;;; This is the simple LAML style based on the html4.0-loose libraries.
;;;; Using this style, a number of useful LAML libraries will be loaded (time, color, file-read, cgi and of course the mirror libraries basis, surface and convenience).
;;;; In addition, the simple style defines the function generic-page (in two variants generic-page and generic-page-1) which
;;;; are useful to make a simple WWW page with title, body and color specification.

; OBSOLETE - use the expressions (source-filename-without-extension) instead (later binding).
(define laml-source-file-name-without-extension  (source-filename-without-extension))

; OBSOLETE - use the expressions (startup-directory) instead (later binding).
(define destination-directory 
  (let ((source-dir (startup-directory scheme-system)))
    (if source-dir source-dir "")))

;; Define the name of the generated html file.
;; Using the Emacs activation of LAML the file name is taken from the buffer name, and as a consequence
;; it is not necessary to use this function.
;; Using other means of activation, this function can be used to set the name of the generated html file.
;; File is without extension.
(define (set-file-name! file)
  (set! laml-source-file-name-without-extension file))

(define target-extension "html")

(define cgi-testing #f)

; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")
(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

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
  (if (not laml-source-file-name-without-extension)
      (laml-error "LAML Error: The generic pages procedures can only be used if you work on a laml buffer which is stored in file"))
  (write-text-file
    (page-1 title '()
          (con body (vertical-space 2) (last-update))
          bg-color text-color link-color vlink-color)
    (string-append destination-directory laml-source-file-name-without-extension ".html"))))

(define (meta-tag-clauses)
   (list
     (list 'name "Generator" 'content "LAML")
   )
)

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



