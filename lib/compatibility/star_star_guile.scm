; The LAML library and programs written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2004 Kurt Normark.
; Some parts of this file (for version 14) has been addapted from Thien-Thi Nguyen's version 12 port of LAML to guile.
; A patch from Thien-Thi Nguyen has been applied Feb 6, 2004 (on version 24).
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


;;;; Guile specific stuff to be loaded for compatibility.  Works only for Guile 1.6 and newer versions (due to eval which takes two parameters).
;;;; This file implements each of the necessary non-R4RS and non-R5RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in MzScheme.


;;; Definition of non-R4RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R4RS).

;; current-time exists in guile
;; sort-list exists in guile
;; file-exists? exists in guile
;; copy-file exits in guile

(define (directory-exists? name)
  (and (file-exists? name) (file-is-directory? name)))

;; Make a new directory, new-dir, in the directory path (first parameter).
;; The parameter in-directory-path ends in a slash.
(define (make-directory-in-directory in-directory-path new-dir)
  (mkdir (in-vicinity in-directory-path new-dir))) 

;; Return the directory list of the path.
(define (directory-list path)
  (let* ((stream (opendir path))
         (next (lambda () (readdir stream))))
    (let loop ((entry (next)) (acc '()))
      (cond ((eof-object? entry)
             (closedir stream)
             (reverse acc))
            ((or (string=? "." entry) (string=? ".." entry))
             (loop (next) acc))
            (else
             (loop (next) (cons entry acc)))))))

; ---------------------------------------------------------------------------------------------------


;; Mail sending support: Send an email to a receiver with title and contents.
;; The optional parameter temp-dir gives a temporary directory used for the mail sending; default is "temp/"
;; This particular implementation requires the function write-text-file from lib/file-read.scm
(define (mail receiver title contents . temp-dir)
  (use-modules (ice-9 popen))
  (let ((p (open-output-pipe (format #f "mail -s '~A' ~A" title receiver))))
    (display contents p)
    (force-output p)
    (close-pipe p)))


(define bound? defined?)

; For Guile version 1.4:
; (define (eval-cur-env e)
;   (eval e))

; Evaluate the expression e in the current environment.
; For Guile version 1.6 and later.
(define (eval-cur-env e)
 (eval e (interaction-environment)))


; -----------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.


;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght three, or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (witout extension and initial path).
;; Element number three must be a slash terminated directory, in the source file resides.
;; Element number four must be a list of program parameters.
;; This function must be redefined in scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (let ((cmd-line (command-line))) 
    (cond ((= (length cmd-line) 3)   ; A list such as ("/usr/bin/guile" "index" "/user/normark/scheme/temp/")
             (list 'laml (file-name-proper (cadr cmd-line)) (caddr cmd-line) '()))
          ((= (length cmd-line) 4)   
             (list 'laml (file-name-proper (cadr cmd-line)) (caddr cmd-line) (cadddr cmd-line) ))
          (else #f))))

;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; Source-file must be a file name without initial path and without extension.
;; start-dir must be an absolute path to a directory ending in a slash /.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in scheme-system dependent compatibility file
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (let ((program-parameters (optional-parameter 1 optional-parameter-list '())))
    (set! command-line (lambda () (list 'laml source-file startup-dir program-parameters)))))

; -----------------------------------------------------------------------------
; We wish to use case sensitive Guile, and NOT  (read-enable 'case-insensitive)

; Increase stack space area:
(debug-set! stack 200000)

; Provide for R5RS Macros in Guile:
(use-syntax (ice-9 syncase))