; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2000,  Kurt Normark, normark@cs.auc.dk.
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


;;;; This is a Lisp pretty printing library, which especially is oriented towards Scheme.<p>
;;;; The pretty printer handles all aspects of Scheme which has special lexical syntax: lists, pairs (dot notation), booleans, chars,
;;;; vectors, and quasi quotation (backquoting). In addition, the pretty printing function handles Scheme special forms
;;;; such as cond, let, define, if, etc. <p>
;;;; A few parameters (global variables) control the pretty printer.<p>
;;;; Conventional comments, read by the this pretty printer, are lost.
;;;; This library assumes that comments are represented as syntactical forms like (comment!!! n "A comment").
;;;; The LAML schemedoc tool contains a procedure lexical-to-syntactical-comments! which converts conventional, semicolon Lisp
;;;; comments to the syntactical comments expected by the pretty printing library.
;;;; The variable syntactical-comment-symbol (and the variable COMMENT-FORM-START in SchemeDoc) allows you to control
;;;; the prefix symbol of syntactical comments.<p>
;;;; The important top level functions of this library are pretty-print-lisp-file and pretty-print-lisp-form. <p>
;;;; Internally, the pretty printer adds pretty-print tokens (strings or chars) to a list, which finally is reversed and linearized
;;;; into a string.<p>
;;;; This library relies on the general library (not loaded by this library).<p>
;;;; In the laml.scm file there are LAML procedures <a href = "../../man/laml.html#scheme-pp">scheme-pp</a> 
;;;; and <a href="../../man/laml.html#scheme-pp-simple">scheme-pp-simple</a> which pretty print Scheme and Lisp files.
;;;; <kbd>pp-scheme</kbd> handles comments properly. These are the most top-level Scheme pretty printing procedures in the 
;;;; LAML system.
;;;; .title Reference Manual of the Scheme Pretty Printing Library

;;; Pretty printing parameters.
;;; The variables in this section controls the pretty printing.

;; An integer which gives the level of indentation
(define indentation-delta 3)

;; A boolean which controls the application of single line pretty printing.
;; If true, the Lisp pretty printer will pretty print short list forms on a single line
(define use-single-lining #t)

;; An integer that expresses the preferred maximum column width
(define prefered-maximum-width 90)

;; The symbol which is used for syntactical comments, such as (comment 2 "This is a comment").
;; It is recommended to use a symbol which is unlikely to be used in 'normal programs', such as comment!!!
(define syntactical-comment-symbol 'comment!!!)

;;; Top level pretty printing functions.
;;; In this section you will find the important, overall pretty printing functions.

;; Assume that in-file-path contains a lisp file with one or lisp forms.
;; Pretty prints the Lisp forms taken from in-file-path.
;; Output them on the optional out-file-path (which per default is in-file-path). <br>
;; Important warning: If only one file is given as parameter, the input file is overwritten by the pretty printed result.
;; .form (pretty-print-lisp-file in-file-path [out-file-path])
(define (pretty-print-lisp-file in-file-path . optional-parameters)
 (let ((out-file-path (optional-parameter 1  optional-parameters in-file-path)))
  (let ((form-list (file-read-all in-file-path))
       )
   (write-text-file 
    (source-aggreated-text-with-interspacing
     (map pretty-print-lisp-form form-list)
     form-list)
    out-file-path))))

(define (source-aggreated-text-with-interspacing pp-res-list form-list)
  (source-aggreated-text-with-interspacing-1 pp-res-list form-list ""))

(define (source-aggreated-text-with-interspacing-1 pp-res-list form-list res)
  (if (null? pp-res-list)
      res
      (let ((this-pp-contr (car pp-res-list))
            (this-form (car form-list))
            (double-cr (string-append (as-string #\newline) (as-string #\newline)))
           )
        (source-aggreated-text-with-interspacing-1 (cdr pp-res-list) (cdr form-list)
           (if (syntactical-comment-form? this-form)
               (string-append res this-pp-contr)
               (string-append res this-pp-contr double-cr))))))


    
;; Pretty print form and return the pretty printed string
(define (pretty-print-lisp-form form)
 (set! res '())
 (pretty-print-lisp-form-1 form 0 #f)
 (linearize-pp-result (reverse res)))

;; Pretty print the string str and the return pretty printed string.
;; This function is not efficient, because it generates a temporary file, from which we parse the string,
;; and after this the function pretty-print-lisp-form is used.
(define (pretty-print-lisp-string str)
 (let ((temp-file-path (string-append (laml-temp-file-path) "lisp-form-string.lsp")))
   (write-text-file str temp-file-path)
   (let ((form (file-read temp-file-path)))
     (pretty-print-lisp-form form))))


; -------------------------------------------------------------------
; The internal result list

(define res '())

(define (add-to-res x)
  (set! res (cons x res)))

(define (remove-from-res i)
  (if (> i 0)
      (begin
        (set! res (cdr res))
        (remove-from-res (- i 1)))))

(define (linearize-pp-result lst)
 (apply string-append
  (map as-string lst)))

(define (add-white-space single-lining?)
  (if single-lining? 
      (add-to-res #\space)
      (add-to-res #\newline)))

; ------------------------------------------------------------------
; Internal Lisp pretty-printing stuff

; The central, internal lisp pretty printing function.
(define (pretty-print-lisp-form-1 form start-col single-lining?)
  (cond ((symbol? form) (add-to-res (as-string form)))
        ((number? form) (add-to-res (as-string form)))
        ((string? form) (add-to-res (string-it (string-quote-protect form))))
        ((char? form) (add-to-res (char-it form)))       ; scheme specific
        ((boolean? form) (add-to-res (bool-it form)))    ; scheme specific
        ((vector? form) (vector-it form start-col single-lining?))
        ((quote-form? form)  ; (quote x)  
           (add-to-res "'")
           (pretty-print-lisp-form-1 (cadr form) (+ 1 start-col) single-lining?))
        ((quasiquote-form? form)  ; (quasiquote x) - backquote
           (add-to-res "`")
           (pretty-print-lisp-form-1 (cadr form) (+ 1 start-col) single-lining?))
        ((unquote-form? form)  ; (unquote x) or ,x
           (add-to-res ",")
           (pretty-print-lisp-form-1 (cadr form) (+ 1 start-col) single-lining?))
        ((unquote-splicing-form? form)  ; (unquote-splicing x) or ,@x
           (add-to-res ",@")
           (pretty-print-lisp-form-1 (cadr form) (+ 2 start-col) single-lining?))
        ((syntactical-comment-form? form) ; (comment x)
           (pretty-print-syntactical-comment form start-col single-lining?))
        ((special-form? form)  ; let, cond, define and others that call for special pretty priting
           (pretty-print-special-form form start-col)) 
        ((list? form)
           (cond ((null? form) (add-to-res "()"))
                 ((single-liner-form? form start-col prefered-maximum-width)
                       (add-to-res "(")
                       (add-list-rest-to-res-single-liner form)
                       (add-to-res ")"))
                 (else (add-to-res "(")
                       (pretty-print-lisp-form-1 (car form) start-col single-lining?)
                       (add-white-space single-lining?)
                       (if (not single-lining?) (add-to-res (indentation (+ start-col indentation-delta))))
                       (add-list-rest-to-res (cdr form) (+ start-col indentation-delta) single-lining?)
                       (add-to-res ")"))))
        ((pair? form)
           (let ((prefix-list (proper-part form))
                 (last-el (first-improper-part form)))
            (cond ((single-liner-form? form start-col prefered-maximum-width)
                       (add-to-res "(")
                       (add-list-rest-to-res-single-liner prefix-list)
                       (add-to-res " . ")
                       (let ((dummy 0))
                         (pretty-print-lisp-form-1 last-el dummy #t))
                       (add-to-res ")"))
                  (else 
                      (add-to-res "(")
                      (add-list-rest-to-res prefix-list (+ start-col 2) single-lining?)
                      (add-to-res " . ") 
                      (add-white-space single-lining?)
                      (if (not single-lining?) (add-to-res (indentation (+ start-col 2))))
                      (pretty-print-lisp-form-1 last-el (+ start-col 2) single-lining?)
                      (add-to-res ")")))))
   )
)


(define (quote-form? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'quote)))

(define (quasiquote-form? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'quasiquote)))

(define (unquote-form? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'unquote)))

(define (unquote-splicing-form? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'unquote-splicing)))

(define (syntactical-comment-form? x)
  (and (list? x) (= 3 (length x)) (eq? (car x) syntactical-comment-symbol)))

(define (special-form? x)
  (and (list? x) (not (null? x)) (memq (car x) '(define if let let* letrec cond))))

(define (named-let-form? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'let ) (symbol? (cadr x))))

; Is form x going to pretty printed on a single line.
; We are in start-col, and the preferred maximum column widht is max-width
(define (single-liner-form? x start-col max-width)
  (if use-single-lining
      (let ((width (meassure-lisp-form x)))
        (<= (+ width start-col) max-width))
      #f))

; Prefix string quotes within str with backslashes
(define (string-quote-protect str)
  (replace-string str (as-string #\") (string-append (as-string #\\) (as-string #\"))))


; return the size of form assumed that it is pretty print in single-spacing mode.
(define (meassure-lisp-form form)
  (cond ((symbol? form) (string-length (as-string form)))
        ((number? form) (string-length (as-string form)))
        ((string? form) (+ 2 (string-length (as-string form))))
        ((char? form) 3)       ; not exact meassure
        ((boolean? form) 2)    
        ((vector? form) (+ 1 (meassure-lisp-form (vector->list form))))
        ((quote-form? form) (+ 1 (meassure-lisp-form (cadr form))))
        ((quasiquote-form? form)  ; (quasiquote x) - backquote
           (+ 1 (meassure-lisp-form (cadr form))))
        ((unquote-form? form)  ; (unquote x) or ,x
           (+ 1 (meassure-lisp-form (cadr form))))
        ((unquote-splicing-form? form)  ; (unquote-splicing x) or ,@x
           (+ 2 (meassure-lisp-form (cadr form))))
        ((list? form)
           (let ((meassure-list (map meassure-lisp-form form)))
             (+ (accumulate-right + 0 meassure-list) (length form) 1)))
        ((pair? form)
           (let ((prefix-list (proper-part form))
                 (last-el (first-improper-part form)))
             (+ (meassure-lisp-form prefix-list) 3 (meassure-lisp-form last-el))))
        (else 0)
   ))


; Emit a Scheme char surface form of ch
(define (char-it ch)
  (cond ((eqv? ch #\space) (string-append (as-string #\#) (as-string #\\) "space"))
        ((eqv? ch #\newline) (string-append (as-string #\#) (as-string #\\) "newline"))
        (else (string-append (as-string #\#) (as-string #\\) (as-string ch)))))

; Emit a Scheme boolean surface form of b
(define (bool-it b)
  (if b "#t" "#f"))

(define (vector-it form start-col single-lining?)
  (add-to-res "#")
  (pretty-print-lisp-form-1 (vector->list form) (+ 1 start-col) single-lining?)
)
  
; Process the rest of a list
(define (add-list-rest-to-res list-tail start-col single-lining?)
  (for-each
    (lambda (el)
      (pretty-print-lisp-form-1 el start-col single-lining?)
      (add-white-space single-lining?)
      (if (not single-lining?) (add-to-res (indentation start-col))))
    list-tail)
  ; remove lst newline and indentation
  (remove-from-res (if single-lining? 1 2)))

(define (add-list-rest-to-res-single-liner lst)
 (let ((dummy 0))
  (for-each
    (lambda (el)
      (pretty-print-lisp-form-1 el dummy #t)
      (add-to-res #\space))
    lst)
  (remove-from-res 1) ; last space removed
 )
)

(define (indentation n)
  (make-string n #\space))

; always ends i a newline to make it impossible that trailing lisp stuff ends up in a comment
(define (pretty-print-syntactical-comment form start-col single-lining?)
 (let* ((level (as-number (cadr form)))
        (comment-prefix (string-append (make-string level #\;) (as-string #\space)))
        (cr-comment-prefix (string-append (as-string #\newline) comment-prefix))
        (comment-text (caddr form))
        (comment-text-1 (replace-string comment-text (as-string #\newline) cr-comment-prefix)) 
       )
    (add-to-res comment-prefix) 
    (add-to-res comment-text-1)
    (add-to-res #\newline)))

(define (pretty-print-special-form form start-col)
  (let ((which-form (car form)))
    (cond ((and (eq? which-form 'define) (>= (length form) 2))
              (pretty-print-define-form form start-col))
          ((named-let-form? form)
              (pretty-print-named-let-form form start-col))
          ((and (memq which-form '(let let* letrec)) (>= (length form) 2))
              (pretty-print-let-form form which-form start-col))
          ((and (eq? which-form 'cond) (>= (length form) 2))              
              (pretty-print-cond-form form start-col))
          ((and (eq? which-form 'if) (>= (length form) 3))              
              (pretty-print-if-form form start-col))
          (else (error (string-append "pretty-print-special-form: Unknown special form: " (as-string which-form)))))))

(define (pretty-print-define-form form start-col)
 (if (single-liner-form? form start-col prefered-maximum-width)
     (let ((dummy 0)) 
      (add-to-res "(")
      (add-to-res "define")
      (add-to-res #\space)
      (add-list-rest-to-res (cdr form) dummy #t)
      (add-to-res ")")
    )
    (let ((name-or-call-form (cadr form))
         (body-rest (cddr form))
         (new-indent (+ start-col indentation-delta))
         ) 
      (add-to-res "(")
      (add-to-res "define")
      (add-to-res #\space)
  
      (pretty-print-lisp-form-1 name-or-call-form (+ start-col 8) #f)  ; #t if we insist on calling form on a single line
      (add-to-res #\newline)
      (add-to-res (indentation new-indent))
  
      (add-list-rest-to-res body-rest new-indent (single-liner-form? body-rest new-indent prefered-maximum-width))
      (add-to-res ")") )))

(define (pretty-print-let-form form which-let-form start-col)
  (let ((name-bindings (cadr form))
        (which-let-form-string (as-string which-let-form))
        (body-rest (cddr form))
        (new-indent (+ start-col indentation-delta))
       )
    (add-to-res "(")
    (add-to-res which-let-form-string)
    (add-to-res #\space)

    ; #f: name bindings never on same line:
    (pretty-print-lisp-form-1 name-bindings (+ start-col (string-length which-let-form-string)) #f)
    (add-to-res #\newline)
    (add-to-res (indentation new-indent))

    (add-list-rest-to-res body-rest new-indent (single-liner-form? body-rest new-indent prefered-maximum-width))
    (add-to-res ")") ))

(define (pretty-print-named-let-form form start-col)
 (let* ((name (cadr form))
        (name-bindings (caddr form))
        (body-rest (cdddr form))
        (name-val-indent (+ start-col 6 (string-length (as-string name))))
        (body-indent (+ start-col indentation-delta))
       )
    (add-to-res "(")
    (add-to-res "let")
    (add-to-res #\space)
    (add-to-res (as-string name))
    (add-to-res #\space)

    ; #f: name bindings never on same line:
    (pretty-print-lisp-form-1 name-bindings name-val-indent #f)
    (add-to-res #\newline)
    (add-to-res (indentation body-indent))

    (add-list-rest-to-res body-rest body-indent (single-liner-form? body-rest body-indent prefered-maximum-width))
    (add-to-res ")") ))
  

(define (pretty-print-cond-form form start-col)
  (let ((condition-consequence-rest (cdr form))
       )
    (add-to-res "(")
    (add-to-res "cond")
    (add-to-res #\space)

    (add-list-rest-to-res condition-consequence-rest (+ start-col 6) #f)
    (add-to-res ")") ))

(define (pretty-print-if-form form start-col)
  (if (single-liner-form? form start-col prefered-maximum-width)
      (let ((condition (cadr form))
            (body-rest (cdr form))
            (dummy 0))
        (add-to-res "(")
        (add-to-res "if")
        (add-to-res #\space)
        (add-list-rest-to-res body-rest dummy #t)
        (add-to-res ")") )
      (let ((condition (cadr form))
            (body-rest (cddr form))
            (new-indent (+ start-col 4)))
        (add-to-res "(")
        (add-to-res "if")
        (add-to-res #\space)
        
        (pretty-print-lisp-form-1 condition new-indent (single-liner-form? condition new-indent prefered-maximum-width))
        (add-to-res #\newline)
        (add-to-res (indentation new-indent))

        (add-list-rest-to-res body-rest new-indent #f)
        (add-to-res ")") )))


                      