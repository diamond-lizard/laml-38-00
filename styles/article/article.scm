;;;; An LAML Article page style.
;;;; This style is for authoring a relatively simple scientific paper in LAML.
;;;; The article style is modelled after LaTeX, but it is still incomplete and
;;;; ad hoc. I include stuff when I need it. Simple formatting can be done via HTML tags mirrored in LAML.
;;;; The alternative style, article-latex, generates LaTeX instead of HTML.
;;;; .title Manual of the LAML Article Style

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999  Kurt Normark, normark@cs.auc.dk.
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

; COPYRIGHT (c)  Kurt Nørmark, Department of Computer Science, Aalborg University, Denmark.
; normark@cs.auc.dk,  http://www.cs.auc.dk/~normark/

; ---------------------------------------------------------------------------------------------------
; LOADING

; Assumes that laml.scm is preloaded by the context of this style.

; general.scm is loaded by laml.scm
;(lib-load "cgi.scm")

(lib-load "html4.0-loose/basis.scm")
(lib-load "html4.0-loose/surface.scm")
(lib-load "html4.0-loose/convenience.scm")
(lib-load "hex.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; VARIABLES WHICH HOLD ARTICLE CONSTITUENTS

(define laml-source-file-name-without-extension (source-filename-without-extension))
(define article-title "Article")
(define article-author "Author")
(define article-affiliation '())
(define article-abstract "")
(define home-url #f)

(define article-name laml-source-file-name-without-extension)

(define article-section-list '()) ; the list in which we collect article section descriptions
(define bibliography-item-list '())

(define aux-info '())

; -----------------------------------------------------------------------------
; Setter functions:

(define (set-article-name! name)
  (set! article-name name))

(define (set-article-author! author-and-affiliation)
  (if (and (list? author-and-affiliation) (>= (length author-and-affiliation) 1))
      (set! article-author (car author-and-affiliation))
      (set! article-author "Author")))

(define (set-article-affiliation! author-and-affiliation)
  (if (and (list? author-and-affiliation) (>= (length author-and-affiliation) 1))
      (set! article-affiliation (cdr author-and-affiliation))
      (set! article-affiliation (list "Affiliation"))))

(define (set-article-abstract! abstract)
  (set! article-abstract abstract))

(define (set-article-title! title)
  (set! article-title title))

(define (set-home-url! url)
  (set! home-url url))

; ---------------------------------------------------------------------------------------------------
;;; Colors and other settings.
;;; In this section we find the background, foreground, linking, and title colors of the article style.
;;; Furthermore, additional settings are provided for.

;; The background color of the article
(define article-background-color white)

;; The foreground color of the article
(define article-foreground-color black)

;; The color of links from the article
(define article-link-color black)

;; The color of visited links from the article
(define article-vlink-color black)

;; The color of the article title
(define article-title-color blue)

;; The color of header lines in the article
(define article-header-color blue)

;; The color of index entries in the table of contents
(define article-index-color grey1)

;; A boolean variable which controls generation of table of contents (index).
(define show-index? #t)

;; In case of show-index, this variable gives the number of columns in the table of contents
(define index-columns 3)

; ---------------------------------------------------------------------------------------------------
;;; Top level function. 
;;; In this section the important top level functions of the article style are presented. 

;; Define the article title
(define (article-title title)
  (set-article-title! title))

;; Define the article author and affiliation. 
;; The first of the parameters is usually the author name. The remaining parameters
;; represent affiliation information.
(define (article-author-and-affiliation . author-and-affiliation)
  (set-article-author! author-and-affiliation)
  (set-article-affiliation! author-and-affiliation))

;; Define the article abstract
(define (article-abstract abstract)
  (set-article-abstract! abstract))

;; Define a section in this article. 
;; The first parameter, id, is a symbol which identifies the section.
;; The seconde parameter, title, is a title string. The last parameter
;; is the body of a section, typically and article-paragraph.
(define (article-section id title section-body)
  (set! section-number (+ section-number 1))
  (set! subsection-number 0)

  (set! article-section-list 
    (cons 
     (make-sectional-entry 'section id (list section-number) title section-body)
     article-section-list)))

;; Define a subsection in this article. 
;; The first parameter, id, is a symbol which identifies the section.
;; The seconde parameter, title, is a title string. The last parameter
;; is the body of a section, typically and article-paragraph.
(define (article-subsection id title section-body)
  (set! subsection-number (+ subsection-number 1))

  (set! article-section-list 
    (cons 
     (make-sectional-entry 'subsection id (list section-number subsection-number) title section-body)
     article-section-list)))

;; Define a number of paragraphs. 
;; Paragraphs are separated by a p tag.
(define (article-paragraphs . paragraph-list)
  (let* ((lgt (length paragraph-list))
         (separator (p))
         (separator-list (make-list (- lgt 1) separator)))
    (string-merge paragraph-list separator-list)))

;; Define an article figure (gif or jpg) in terms of an id, a relative file path, and a caption. 
;; The first parameter, id, is a symbol which identifies the figure.
;; The second parameter, relative-file-path, is the relative file path to the jpg or gif file.
;; The third parameter is the caption of the figure (a string).
(define (article-figure id relative-file-path caption . unsed-rest)
  (set! next-figure-number (+ 1 next-figure-number))
  (set! figure-list 
        (cons (make-aux-entry 'figure id next-figure-number caption)
        figure-list))

  (center
   (con
    (p-1) 
    (a-name (as-string id))
    (img-0 (string-append relative-file-path ".jpg")) (p-1)
    (narrow-with-pixels 35 
      (em (string-append "Figure " (as-string next-figure-number) ". " (horizontal-space 2) caption))))))


;; Define a number of name issues, which are rendered as titled items in a list.
;; The parameters are supposed to be issue forms.
(define (article-named-issue-list . issue-list)
  (let* ((lgt (length issue-list))
         (separator (p))
         (separator-list (make-list (- lgt 1) separator))
        )
    (indent-pixels 10
     (string-merge
       (map format-issue issue-list)
       separator-list))))

;; Define an issue of an article-named-issue-list.
;; An issue is defined in terms of a title and a body.
(define (issue issue-title issue-body)
  (list 'issue issue-title issue-body))

;; Define a number of items.
;; The parameters are supposed to be item forms.
(define (article-items . items)
 (let ((items-1 (map (lambda (i) (con i (p))) items)))
  (ul-1 items-1)))

;; Define an item in an article-items form.
(define (item i) i)

;; The clause which starts the article proper.
;; This clause must be ended by end-article.
;; Put article-title, article-author-and-affiliation, article-abstract, the sections, and the article-bibliography
;; between begin-article and end-article.
(define (begin-article)
  (let* ((aux-file (aux-file-name article-name))
         (aux-contents (if (file-exists? aux-file)
                          (file-read aux-file)
                          '())))
    (set! aux-info aux-contents)))


;; The clause which ends the article.
(define (end-article)
  (file-write 
    (append (map trim-section-list-for-aux-format (reverse article-section-list))
            figure-list)
    (aux-file-name article-name))
  (write-text-file
    (page article-title
          (the-article)
          article-background-color
          article-foreground-color
          article-link-color
          article-vlink-color)
    (string-append (startup-directory) article-name ".html")))


;; This form returns a link (anchor tag) to a section or subsection of the article.
;; The reference is resolved via the aux file.
(define (section-ref id)
 (let* ((id1 (as-symbol id))
        (id-resolution 
         (find-in-list (lambda (e) (eq? (get-id-of-entry e) id1)) aux-info)))
  (html4:a
    (if id-resolution (format-reference-number (get-number-list-of-entry id-resolution)) (b "??"))
    'href (string-append "#" (as-string id))
    'title (if id-resolution (get-title-of-entry id-resolution) ""))))

;; This form returns a link to a figure of the article.
(define (figure-ref id)
 (let* ((id1 (as-symbol id))
        (id-resolution 
         (find-in-list (lambda (e) (eq? (get-id-of-entry e) id1)) aux-info)))
  (html4:a
    (if id-resolution (format-reference-number (get-number-list-of-entry id-resolution)) (b "??"))
    'href (string-append "#" (as-string id))
   'title (html-protect (if id-resolution (get-title-of-entry id-resolution) ""))
    )))

;; This form returns a list of links to citations of this article.
;; key-list is a list of strings, which identify bibliograph items.
(define (cite . key-list)
  (con "[" 
        (list-to-string (map format-citation key-list) ",")
       "]"))


; Assume for now that all bib items are cited

;; Define the bibliograph items of the article. 
;; It is assumed that the constituents are bib-item forms.
(define (article-bibliography . bib-items)
  (set! bibliography-item-list bib-items))
  
;; The individual bibliograph items of the article-bibliography.
;; A bib-item is defined in terms of a key and a string, which represents the formatted bib information.
(define (bib-item key formatted-bib)
  (list key formatted-bib))

; ---------------------------------------------------------------------------------------------------
; Helping functions to top level forms

(define section-number 0)
(define subsection-number 0)
(define next-figure-number 0)
(define figure-list '())
(define cite-list '())
(define next-citation-number 0)

(define (make-aux-entry kind id number title)
  (list kind id number title))

(define (make-sectional-entry unit id number-list title body)
  (list unit id number-list title body))

(define get-unit-of-entry (make-selector-function 1))
(define get-id-of-entry (make-selector-function 2))
(define get-number-list-of-entry (make-selector-function 3))
(define get-title-of-entry (make-selector-function 4))
(define get-body-of-entry (make-selector-function 5))

(define (format-issue issue)
  (let ((issue-header (second issue))
        (issue-body (third issue)))
    (con (b issue-header) ": " issue-body)))

(define (the-article)
  (con 
    (center (font-1 6 article-title-color article-title)) (p)

    (center (i (font-1 5 article-foreground-color article-author))) (p)

    (center (font-1 4 article-foreground-color
               (list-to-string article-affiliation (con (br))))) (p)

    (narrow-with-pixels 35 (i article-abstract)) (p)

    (if show-index?
        (present-sectional-index)
        "")

    (list-to-string
      (map format-section-unit (reverse article-section-list))
      (p))

    (vertical-space 2)

    (if (> (length cite-list) 0)
        (con 
          (a-name "bibliography")
          (font-1 5 article-header-color "Bibliography") (p)
          (format-bibliography bibliography-item-list)
        )
        "")

    (vertical-space 2)
    
    (when-generated)

    (vertical-space 25)
  )
)

(define (format-bibliography item-list)
  (list-to-string 
    (map format-bib-item item-list)
    ""))

(define (format-bib-item bib-item)
  (let* ((bib-key (car bib-item))
         (bib-body (cadr bib-item))
         (cite-entry (find-in-list (lambda (cite-entry) (equal? bib-key (car cite-entry))) cite-list))
         (cite-number-string (con (if cite-entry (as-string (cadr cite-entry)) "??") ". "))
        )
   (con 
     (a-name (as-string bib-key))
     (table-3 0 (list 20 "*")
       (list (list cite-number-string bib-body)))
   )))

(define (when-generated)
 (let* ((dt (date-time (current-time)))
        (wd (weekday (current-time)))
        (date (car dt))
        (time (cadr dt)))
  (font-1 2 red (con "Generated: " wd " " date ", " time))))

(define (format-section-unit sectional-entry)
  (let (
        (unit        (get-unit-of-entry sectional-entry))
        (id          (get-id-of-entry sectional-entry))
        (number-list (get-number-list-of-entry sectional-entry))
        (title       (get-title-of-entry sectional-entry))
        (body        (get-body-of-entry sectional-entry))
       )
    (con
      (a-name (as-string id))
      (font-1 5 article-header-color 
       (con (format-reference-number number-list) "." (horizontal-space 3) title)) (br)

      (font-color article-foreground-color body))))

; input: a list of integers or an integer.
; output: a string
(define (format-reference-number number-spec)
  (cond ((number? number-spec) (as-string number-spec))
	((list? number-spec)  (list-to-string
			       (map as-string number-spec)
			       "."))
        (else "??")
  )
)

(define (aux-file-name article-name)
  (string-append article-name "." "laux"))  ; laux in order not to interfere with latex's aux files
          

(define (present-sectional-index)
  (let ((sections (filter (lambda (e) (eq? 'section (get-unit-of-entry e))) aux-info)))
    (if (not (null? sections))
        (con 
          (p) (hr-1)
          (n-column-list index-columns (map index-entry sections) 1000) 
          (hr-1) (p))
        "")))

(define (index-entry sectional-entry)
  (let (
        (unit        (get-unit-of-entry sectional-entry))
        (id          (get-id-of-entry sectional-entry))
        (number-list (get-number-list-of-entry sectional-entry))
        (title       (get-title-of-entry sectional-entry))
       )
    (a-tag 
     (string-append "#" (as-string id))
     (font-1 2 article-index-color (con (format-reference-number number-list) "." " " title)))))


(define (trim-section-list-for-aux-format sectional-entry)
  (let (
        (unit        (get-unit-of-entry sectional-entry))
        (id          (get-id-of-entry sectional-entry))
        (number-list (get-number-list-of-entry sectional-entry))
        (title       (get-title-of-entry sectional-entry))
       )
   (list unit id number-list title)))

(define (format-citation key)
  (let ((existing-citation-entry (find-in-list (lambda (cite-entry) (equal? key (car cite-entry))) cite-list)))

    (if existing-citation-entry		; key already cited once in this article - use same citation number
      (let ((citation-number (cadr existing-citation-entry)))
       (html4:a
	(as-string citation-number)
	'href (string-append "#" (as-string key))
	))
      (begin
       (set! next-citation-number (+  1 next-citation-number))
       (set! cite-list 
	     (cons (make-cite-entry key next-citation-number) cite-list))

       (html4:a
	(as-string next-citation-number)
	'href (string-append "#" (as-string key))
	))
      )
   )
)

(define (make-cite-entry key number)
  (list key number))


; ---------------------------------------------------------------------------------------------------------------
;;; Target specific insertions. Do only use these in special situations.

; Insert insertion if we generate latex
(define (latex-only insertion)
  "")

; Insert insertion if we generate html.
(define (html-only insertion)
  insertion)

; ---------------------------------------------------------------------------------------------------------------



(laml-welcome)