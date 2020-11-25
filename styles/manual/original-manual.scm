; LAML Manual page style.
; The original LAML manual facility which uses the old ad hoc HTML libraries.
; DO NOT USE IT ANY MORE. Instead, use styles/xml-in-laml/manual/manual.scm.

; COPYRIGHT (c)  Kurt Nørmark, Department of Computer Science, Aalborg University, Denmark.
; normark@cs.auc.dk,  http://www.cs.auc.dk/~normark/

; Warning: If schemdoc.scm is loaded after manual.scm, and if manual facility is used
; thereafter, problems occur. There is a conflict, yet unknown, between manual.scm and schemedoc.scm

; ---------------------------------------------------------------------------------------------------
; LOADING

(define (lib-load file-name)
  (load (string-append  the-library "/" file-name)))

(lib-load "general.scm")
(lib-load "cgi.scm")
(lib-load "html.scm")
(lib-load "html-v1.scm")
(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; A variation of as-quoted-string from general which treats subexpressions of the
; form (quote ...) better.
(define (as-quoted-string-1 x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) (string-it x))
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((and (list? x) (not (null? x)) (symbol? (car x)) (eq? (car x) 'quote) (not (null? (cdr x))))
            (string-append "'" 
              (as-quoted-string-1 (cadr x))
            ))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-quoted-string-1 x) (make-list (- (length x) 1) " "))
               ")"))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-quoted-string-1 y) " ")) (proper-part x))
               )
               " . " (as-quoted-string-1 (first-improper-part x))
               ")"))
        (else "??")))

; ---------------------------------------------------------------------------------------------------

; 
; MAINLY PREAMBLE FUNCTION DEFINITIONS

(define laml-source-file-name-without-extension (source-filename-without-extension))

; The explanation of attributes, for which no attribute-description is available
(define standard-attribute-explanation "-")

(define manual-title "manual")
(define manual-author '())
(define manual-abstract "")
(define home-url #f)
(define manual-master-index #f)

(define manual-name (if (not (equal? laml-source-file-name-without-extension ""))
                          laml-source-file-name-without-extension
                          "manual"))

(define (set-manual-name name)
  (set! manual-name name))

(define (set-manual-author . author-list)
  (set! manual-author author-list))

(define (set-manual-abstract abstract)
  (set! manual-abstract abstract))

(define (set-manual-title title)
  (set! manual-title title))

(define (set-home-url url)
  (set! home-url url))

(define (set-manual-master-index url)
 (set! manual-master-index url))


(define manual-page-list '()) ; the list in which we collect manual page descriptions

(define manual-background white)

(define language-preference 'english) ; redefinition

(define laml-manual-stuff #t)   ; true if making a manual for laml-stuff. Affects the header of the generated html file.


; ---------------------------------------------------------------------------------------------------
; Constants

; Do we want the manual style to display information while it processes a manual.
; 0 means silence. 1 means print some info. 
(define manual-verbosity-level 0)

(define attribute-width-list (list 150 250 "*"))

; ---------------------------------------------------------------------------------------------------
;  TOP LEVEL FUNCTION


; for manual sectioning:

;;; Programmatic support of manual generation. 
;;; The functions in the following are selected functions from the style
;;; manual.scm, which are useful when manuals are made from other programs.
;;; This part of the manual has been extracted automatically from the Scheme source file of the style.


; Define a manual page with a variety of different elements
(define (manual-page id . elements)
  (set! manual-page-list 
        (cons (cons (list 'kind "manual-page") elements) manual-page-list))
)

;; Given an entry (or page). Is it a manual page (as opposed to a manual section)
(define (manual-page? elements)
  (let ((res (assq 'kind elements)))
    (if res
        (equal? (cadr res) "manual-page")
        #f)))

;; Given an entry (or page). Is it a manual section (as opposed to a manual page)
(define (manual-section? elements)
  (let ((res (assq 'kind elements)))
    (if res
        (equal? (cadr res) "manual-section")
        #f)))

;; Define a manual section with title and body (plain text)
(define (manual-section . elements)
  (set! manual-page-list
	(cons (cons (list 'kind "manual-section") elements) manual-page-list)))


(define manual-first-sentence-in-string first-sentence-in-string)

; return a reference to another a-named place in the current document
(define (internal-ref anchor)
  (a-tag (this-reference anchor) anchor))

(define (this-reference ref)
  (string-append "#" ref))

; ---------------------------------------------------------------------------------------------------
; TAG GENERATION

(define (make-tag tagid)
  (lambda parameters (cons tagid parameters)))


(define title (make-tag 'title))
(define form (make-tag 'form))
(define pre-condition (make-tag 'pre-condition))
(define description (make-tag 'description))
(define parameters (make-tag 'parameters))
(define parameter (make-tag 'parameter))
(define example (make-tag 'example))
(define cross-references (make-tag 'cross-references))
(define reference (make-tag 'reference))
(define internal-references (make-tag 'internal-references))
(define comment (make-tag 'comment))
(define misc (make-tag 'misc))
(define returns (make-tag 'returns))
(define section-title (make-tag 'section-title))
(define section-body (make-tag 'section-body))

; XML-in-LAML relevant entries:
(define xml-in-laml-example-form (make-tag 'xml-in-laml-example-form))
(define content-model (make-tag 'content-model))
(define attributes (make-tag 'attributes))
(define attribute (make-tag 'attribute))

(define attribute-descriptions (make-tag 'attribute-descriptions))
(define attribute-description  (make-tag 'attribute-description ))

; no user level input for corresponding to library

; the mutual ordering among manual tags:
(define manual-element-order '(section-title section-body title library kind form xml-in-laml-example-form description attributes content-model
                               pre-condition parameters returns example cross-references misc comment))

; the list of possible and supported top level elements of a manual page. Manual section entries are not included in this list.
(define supported-manual-page-elements 
  '(title library kind form description pre-condition parameters example cross-references misc comment returns
          xml-in-laml-example-form attributes content-model))

; ---------------------------------------------------------------------------------------------------
; DISPATCHING STUFF:

(define element-tag car)
(define element-contents cdr) 


(define (find-processor style kind)
  ; return function given the symbols style and kind
 (cond ((eq? style 'plain-manual-style)
        (cond ((eq? kind 'title) plain-manual-title)
              ((eq? kind 'form) plain-manual-form)
              ((eq? kind 'pre-condition) plain-manual-pre-condition)
              ((eq? kind 'description) plain-manual-description)
              ((eq? kind 'parameters) plain-manual-parameters)
              ((eq? kind 'parameter) plain-manual-parameter)
              ((eq? kind 'example) plain-manual-example)
              ((eq? kind 'cross-references) plain-manual-cross-references)
              ((eq? kind 'reference) plain-manual-reference)
              ((eq? kind 'internal-references) plain-manual-internal-references)
              ((eq? kind 'comment) plain-manual-comment)
              ((eq? kind 'misc) plain-manual-misc)
              ((eq? kind 'returns) plain-manual-returns)
              ((eq? kind 'kind) empty-element)
              ((eq? kind 'section-title) plain-manual-section-title)
              ((eq? kind 'section-body) plain-manual-section-body)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) plain-manual-xml-in-laml-example-form)
              ((eq? kind 'content-model) plain-manual-content-model)
              ((eq? kind 'attributes) plain-manual-attributes)
              ((eq? kind 'attribute) plain-manual-attribute)

              ((eq? kind 'attribute-descriptions) plain-manual-attribute-descriptions)
              ((eq? kind 'attribute-description) plain-manual-attribute-description)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in plain-manual-style")))))
       ((eq? style 'plain-secondary-manual-style)
        (cond ((eq? kind 'title) plain-secondary-manual-title)
              ((eq? kind 'form) plain-secondary-manual-form)
              ((eq? kind 'pre-condition) plain-secondary-manual-pre-condition)
              ((eq? kind 'description) plain-secondary-manual-description)
              ((eq? kind 'parameters) plain-secondary-manual-parameters)
              ((eq? kind 'parameter) plain-secondary-manual-parameter)
              ((eq? kind 'example) plain-secondary-manual-example)
              ((eq? kind 'cross-references) plain-secondary-manual-cross-references)
              ((eq? kind 'reference) plain-secondary-manual-reference)
              ((eq? kind 'internal-references) plain-secondary-manual-internal-references)
              ((eq? kind 'comment) plain-secondary-manual-comment)
              ((eq? kind 'misc) plain-secondary-manual-misc)
              ((eq? kind 'returns) plain-secondary-manual-returns)
              ((eq? kind 'kind) empty-element)
              ((eq? kind 'section-title) plain-secondary-manual-section-title)
              ((eq? kind 'section-body) plain-secondary-manual-section-body)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) plain-secondary-manual-xml-in-laml-example-form)
              ((eq? kind 'content-model) plain-secondary-manual-content-model)
              ((eq? kind 'attributes) plain-secondary-manual-attributes)
              ((eq? kind 'attribute) plain-secondary-manual-attribute)

              ((eq? kind 'attribute-descriptions) plain-secondary-manual-attribute-descriptions)
              ((eq? kind 'attribute-description) plain-secondary-manual-attribute-description)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in plain-secundary-manual-style")))))
       ((eq? style 'header-manual-style)
        (cond ((eq? kind 'title) header-manual-title)
              ((eq? kind 'form) header-manual-form)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'description) header-manual-description)
              ((eq? kind 'parameters) empty-element)
              ((eq? kind 'parameter) empty-element)
              ((eq? kind 'example) empty-element)
              ((eq? kind 'cross-references) empty-element)
              ((eq? kind 'reference) empty-element)
              ((eq? kind 'internal-references) empty-element)
              ((eq? kind 'comment) empty-element)
              ((eq? kind 'misc) empty-element)
              ((eq? kind 'returns) empty-element)
              ((eq? kind 'kind) empty-element)
              ((eq? kind 'section-title) empty-element)
              ((eq? kind 'section-body) empty-element)
              ((eq? kind 'library) header-manual-library)

              ((eq? kind 'xml-in-laml-example-form) header-manual-form)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in header-manual-style")))))
       ((eq? style 'toc-manual-style)
        (cond ((eq? kind 'title) empty-element)
              ((eq? kind 'form) empty-element)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'description) empty-element)
              ((eq? kind 'parameters) empty-element)
              ((eq? kind 'parameter) empty-element)
              ((eq? kind 'example) empty-element)
              ((eq? kind 'cross-references) empty-element)
              ((eq? kind 'reference) empty-element)
              ((eq? kind 'internal-references) empty-element)
              ((eq? kind 'comment) empty-element)
              ((eq? kind 'misc) empty-element)
              ((eq? kind 'returns) empty-element)
              ((eq? kind 'kind) empty-element)
              ((eq? kind 'section-title) toc-manual-section-title)
              ((eq? kind 'section-body) empty-element)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) empty-element)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in toc-manual-style")))))
       (else (error "Unknown style"))))


(define (empty-element . pars)
  "")


(define (present-element style kind contents)
  (let ((processing-procedure (find-processor style kind)))
    (apply processing-procedure contents)))


; ---------------------------------------------------------------------------------------------------
; GENERATION OF PREDICATE BASED ON manual-element-order

; generate-leq is in the general library 

(define manual-element-ordering  (generate-leq manual-element-order element-tag))

; ---------------------------------------------------------------------------------------------------
; Primary/secondary elements
; It is possible to decide whether an element is considered as primary or secondary.
; Per default, all element are primary. The function primary-element? may be redefined
; to change the default. The classification can be determined on basis of the fields of
; the manual entry. The primary-element-indication and secondary-element-indication 
; can return a string which is printed after the title to the indicate primary and secondary
; classification. The plain-secondary-manual-style manual style is used to present
; elements classified as secondary elements.

; Is manual-entry considered as a primary-element.
; If not, it is a secondary element.
; Secondary elements can for instance be older elements, which are presented in
; another way as primary elements
; Can be redefined in laml files that produce concrete manuals.
(define (primary-element? manual-entry) #t)

; Primary/secondary element indications to be show after the title.
; Can be redefined in laml files that produce concrete manuals.
(define (primary-element-indication) "")
(define (secondary-element-indicationo) "")


; ---------------------------------------------------------------------------------------------------
; CENTRAL PRESENTATION FUNCTIONS:

; returns a list of html-markup
; elements is the documentation element (a list) of a single manual page
(define (format-manual-page elements primary-style . optional-parameter-list)
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style)))
  (cond ((or (manual-section? elements) (manual-page? elements))  ; the two cases may be separated in the future, if needed
         (map
	  (lambda (e)
	    (present-element
	     (if (primary-element? elements) primary-style secondary-style)
	     (element-tag e)
	     (element-contents e)) 
	    )
	  (sort-list elements manual-element-ordering)))
        (else  (error "format-manual-page: unknown manual entry")))))


(define (string-concat string-list)
  (apply string-append string-list))

(define (line-breaker str)
  (con str (br)))

(define (info-title manual-page)
 (let ((ttl1 (assq 'title manual-page))
       (ttl2 (assq 'section-title manual-page)))
  (cond (ttl1 (cadr ttl1))
        (ttl2 (cadr ttl2))
        (else "????"))))

; manual is a list, such as manual-page-list
; thus manual is the whole manual struture.
; Returns a string a html markup
(define (format-manual manual primary-style . optional-parameter-list)
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style)))
  (string-concat
    (map
      (lambda (manual-page)
        (if (>= manual-verbosity-level 1) (display-message (info-title manual-page)))
        (con 
          (string-concat
           (map line-breaker
            (filter (compose not empty-string?)
             (format-manual-page
               manual-page primary-style secondary-style))))
          (hr)
        )
      )
      manual))))

(define (present-author-info au)
 (let ((au1 (if (not (null? au)) (cons (copyright-owner (car au)) (cdr au)) au)))
   (h 3
      (con 
       (apply con
	      (map (lambda (e) (con e (horizontal-space 4))) au1))
       ))))

(define (present-abstract abstr)
 (let* ((offset 15)
        (width (- (accumulate-right + 0 manual-index-width-list) (* 2 offset))))
   (if (not (equal? abstr "")) (con (indent-pixels (+ 12 offset) (color-frame-width (em abstr) grey2 (- width offset))) (p)) "")  ))


;; This function makes the manual from collected information.
;; In this sense, this is the function which makes things happen.
;; If no first parameter is given, the manual is made from the collected information,
;; which is present in the variable manual-page-list.
;; If a parameter is given, the manual is made from this list of entries.
;; A second parameter it gives the absolute destination-directory.
;; A possible third parameter is the name of the source file. (This parameter is used for
;; informational purposes only).
;; This functions reverses the order of entries.
;; Take a look at one of the lsp files with manual information in order
;; to find out about the list structures of a manual-list.
;; A HTML file is written containing the formatted manual.
;; The HTML file is written to a particular destination-dirctory
;; (if this information is provided as the second parameter), else to the current directory.
(define (make-manual . par-list)
 (let* ((manual-page-list-1 (if (null? par-list) 
                                (reverse manual-page-list)  ; the global variable
                                (reverse (first par-list))  ; the first parameter - par-list is bound to a list of passed parameters...
                            ))
        (manual-page-list-2 (merge-attributes-and-descriptions manual-page-list-1)) ; join attributes and attribute-descriptions
                                                                                    ; by adding attribute description as an extra element
                                                                                    ; to an attribute. Attribute-descriptions stays
                                                                                    ; as an non-presented information in manual-page-list-2


        (manual-page-list-3 (map clean-manual-page manual-page-list-2))   ; remove unkwown fields form manual pages
        (destination-dir (if (>= (length par-list) 2) (second par-list) ""))
        (source-file-name-information (if (>= (length par-list) 3) (third par-list) ""))
        (table-of-contents? (> (number-of-sections? manual-page-list-3) 0))
        (yr (car (time-decode (current-time))))
       )

  (write-text-file
   (page
      manual-title 
      (con
        (a-name "MANUAL-TOP")
        (if laml-manual-stuff 
            (left-middle-right-banner (when-generated) (string-append "Copyright " copyright  (as-string yr) ", Kurt Nørmark")
                                      (if (empty-string? destination-dir)
                                          (laml-home-button 0 "laml-home.gif")
                                          (laml-home-button 0 "laml-home.gif" destination-dir)))

            (left-middle-right-banner "" "" (em (when-generated))))
            
        (font-color blue (h 1 manual-title)) (p) 
        (present-author-info manual-author) (p)
        (if (and #f home-url (not (equal? home-url ""))) (con (a-tag home-url "Home") (space 3)) "")   ; no home page ref any more
        
        (if manual-master-index 
            (con (a-tag manual-master-index "Master index") (br))
            "")

        (if (not (empty-string? source-file-name-information))
            (font-color grey (con "Source file: " source-file-name-information (br)))
            "")
        (font-color grey (con "LAML " laml-version))
  
        (p)

        (present-abstract manual-abstract) (p)



        (if table-of-contents?
          (con 
            (a-name "MANUAL-TOC")
            (em "Table of Contents:")
            (indent-pixels 12 
              (con 
                (manual-table-of-contents manual-page-list-3 'toc-manual-style))) (p))
          "")


        (if (not (null? manual-page-list-3))
          (con
           (a-name "MANUAL-INDEX")
           (em "Alphabetic index:")
          (indent-pixels 12 
              (con 
                   (manual-index manual-page-list-3 'header-manual-style))) (p))
          "")

        
        (format-manual manual-page-list-3 'plain-manual-style 'plain-secondary-manual-style)
        (font 2 red (when-generated)) (br)
        (font 2 red end-remark)
        (vertical-space 25)  ; to allow for scrolling at the end of a manual page
      )
      white black blue blue
     )
   (string-append destination-dir (as-string manual-name) ".html"))

   ; Write the uncleaned manual page to a -.lsp file. This keeps specialized fields for other purposes.
   (if (<= (length par-list) 1)
       (write-lisp-form-manual manual-page-list-1)
       (write-lisp-form-manual manual-page-list-1 destination-dir))))


; For each attribute, find out if there is a separate attribute description. If there is, 
; join it with the attribute record.
(define (merge-attributes-and-descriptions manual-page-list)
  (let ((res (map merge-attributes-and-descriptions-entry manual-page-list)))
     res))


(define (merge-attributes-and-descriptions-entry entry)
 (cond ((manual-page? entry) (merge-attributes-and-descriptions-page entry)) 
       (else entry)))

(define (merge-attributes-and-descriptions-page page)
 (let* ((attributes (defaulted-get 'attributes page '()))
        (attribute-descriptions (defaulted-get 'attribute-descriptions page '()))
        
        ; local selectors:
        (id-of-attribute second)
        (id-of-attribute-descr second)
        (explanation-of-description third)
 
        (new-attributes 
          (map 
 	   (lambda (a) 
	     (let* ((id (id-of-attribute a))
		    (corresponding-description 
		     (find-in-list (lambda (desc) (equal? (id-of-attribute-descr desc) id)) attribute-descriptions))
		    )
	       (if corresponding-description
		   (append a (list (explanation-of-description corresponding-description)))
		   (append a (list standard-attribute-explanation)))))
 	  attributes))
      )
   (cond ((null? attributes) page)
         (else (append 
		(filter (lambda (x) (not (eq? (car x) 'attributes))) page) ; all fields but attributes
		(list (cons 'attributes new-attributes)))))))


;; filter a manual page such that only known elements are processed.
(define (clean-manual-page page)
 (if (manual-page? page)
     (filter 
       (lambda (element) (memq (car element) supported-manual-page-elements)) 
       page)
     page))

    

;; A possible end remark added as the last line of the documentation in small font. Default is the empty string.
(define end-remark "") 

; return a horizontal slize of a manual contribution
; As an optional parameter this function accept an explanation of the attribute
(define (manual-slice left-contribution right-contribution . optional-parameter-list)
 (let ((attribute-explanation (optional-parameter 1 optional-parameter-list #f)))
  (table-1 0 '(12 140 12 "*")  (make-list 4 manual-background) 
	   (list (list ""
		       (con
                         (font-color red (b left-contribution))
                         (if attribute-explanation
                             (con (br) (font-size 1 attribute-explanation))
                             ""))
		       ""
		       right-contribution)) 'top)))

; ---------------------------------------------------------------------------------------------------
;;; Index functions. 
;;; The functions supporing the generation of a manual index.

;; The widths of the colums of the manual index.  A list of three integers
(define manual-index-width-list (list 140 300 400))

;; Return a manual index. Style should be the symbol header-manual-style
(define (manual-index manual style)
  (table-1 1 manual-index-width-list (list white white white)   
    (map
      (lambda (manual-page)
         (filter (lambda (e) (not (equal? e ""))) (format-manual-page (only-one-form manual-page) style)))
      (sort-list 
         (filter manual-page? manual)  ; disregard manual sections for the index
          manual-index-order-leq?))))

; Ensure that only one of form and xml-in-laml-example-form are on manual-page.
; Keep form
(define (only-one-form manual-page)
 (let ((form (get-defaulted-prop 'form manual-page #f)) 
       (xml-in-laml-form (get-defaulted-prop 'xml-in-laml-example-form manual-page #f))
      )
  (cond ((and form xml-in-laml-form)
          (filter (lambda (x) (not (eq? (car x) 'xml-in-laml-example-form))) manual-page))
        (else manual-page))))

;; The predicate ordering the manual entries for index sorting
(define (manual-index-order-leq? x y)
  (string<=? (cadr (assq 'title x)) (cadr (assq 'title y))))

;; Writes a manual structure (list of entries) to a file.
;; The filename is manual-name.lsp.
;; If the optional parameter dir is given write the lsp file in this directory.
;; Else write it in the current directory.
;; If provided, dir ends in a slash. 
;; The lsp file is useful for programmatic access to the entire body of manual information.
(define (write-lisp-form-manual manual-page-list . dir)
  (let* ((dir-1 (if (not (null? dir)) (first dir) ""))
         (lisp-form-manual-file-name (string-append dir-1 (as-string manual-name) ".lsp"))
        )
    (if (file-exists? lisp-form-manual-file-name)
	(delete-file lisp-form-manual-file-name))

    (with-output-to-file lisp-form-manual-file-name
			 (lambda () (write  manual-page-list)))))



; ---------------------------------------------------------------------------------------------------
; MANUAL PLAIN STYLE:

(define (plain-manual-title title-string)
  (con  (a-name title-string) 
        (if (empty-string? (primary-element-indication))
            (h 2 (font-color blue (con title-string)))
            (h 2 (font-color blue (con title-string (horizontal-space 8) (primary-element-indication)))))))

(define (plain-manual-form form-list)
  (manual-slice "Form"
    (box (kbd (as-string form-list)))))

(define (plain-manual-pre-condition precon)
 (manual-slice 
    "Preconditions"
     (box precon)))

(define (plain-manual-description descr)
 (manual-slice 
   "Description"
   (box (capitalize-string-nd (strip-initial-spaces descr)))))

(define (plain-manual-parameters . par-list)
 (manual-slice "Parameters"
   (table-3 0 '(140 5 "*")
    (map
     (lambda (par)
      (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
    par-list) 'top)))


(define (plain-manual-parameter par explanation)
  (list  par ""    explanation) ; fed into a table row
)


(define (plain-manual-example ex)
 (manual-slice 
   "Example" (box (pre (font-size 2 ex)))))

(define (plain-manual-cross-references . ref-list)
 (manual-slice "See also"
   (table-3 0 '(140 5 "*")
    (map
     (lambda (ref)
      (present-element 
        'plain-manual-style
        (element-tag ref)
        (element-contents ref)))
    ref-list) 'top)))


(define (plain-manual-reference ref anchor url)
  (list ref ""  (a-tag url anchor)) ; fed into a table row
)

(define (plain-manual-internal-references ref . ref-list)
  (list ref ""  (string-concat (map (lambda (r) (con (a-tag (string-append "#" r) r) (horizontal-space 4))) ref-list)))
)

(define (plain-manual-comment comment)
 (manual-slice 
   "Internal remark" (box (em (font-size 2 comment)))))


(define (plain-manual-misc m)
 (manual-slice 
    "Note"
     (box m)))

(define (plain-manual-returns r)
 (manual-slice 
    "Returns"
     (box r)))

(define (plain-manual-section-title title-string)
  (let ((section-id (next-section-id)) ; increments next-section-number!
        (number next-section-number)
       )
    (con  (vertical-space 1)
          (a-name section-id)
          (b (font-color blue (con (font-size 6 (con (as-string number) "." (horizontal-space 3))) (font-rise (upcase-string title-string) 5)))))))

(define (plain-manual-section-body body)
  body)


(define (plain-manual-xml-in-laml-example-form f)
 (manual-slice 
    "Example form"
     (box (kbd (as-quoted-string-1 f)))))

(define (plain-manual-content-model cm)
 (manual-slice 
    "XML content model"
     (box (kbd cm))))

(define (plain-manual-attributes . par-list)
 (manual-slice
   "XML Attributes"
   (table-3 0 attribute-width-list 
    (map
     (lambda (par)
      (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
     par-list) 'top)
   (if (null? par-list) 
       ""
       (con "Required: " (font-color red (b "*")) (br)
	    "Default values: " (font-color red "red") ))
 )
)


(define (plain-manual-attribute attr-name attr-type attr-presence-default attribute-explanation)  
 (let* (
        (default-marking (lambda (x) (font-color red x)))
        (attribute-type-present 
          (lambda (at)
           (cond ((string? at) 
                    (string-append 
                      at
                      (if (member attr-presence-default (list "#REQUIRED" "#IMPLIED"))
                          ""
                          (string-append (horizontal-space 5) "(" (font-color red attr-presence-default) ")"))))
                 ((list? at) (string-append "(" (list-to-string (mark-default at attr-presence-default default-marking) " | ") ")"))
                 (else "?????"))))
        (required-attribute? (lambda (ap) (equal? "#REQUIRED" (upcase-string attr-presence-default))))
        (presence-indication 
         (if (required-attribute? attr-presence-default) (font-color red (b " *")) ""))
       )
  (list (string-append attr-name presence-indication)    ; fed into a table row
        (attribute-type-present attr-type)
        (font-size 2 attribute-explanation)) 
 )
)

; Make val in lst with the mark m. Compare elements with equal?
(define (mark-default lst val m)
 (if (not (or (equal? val "#REQUIRED") (equal? val "#IMPLIED")))
     (map (lambda (el) (if (equal? val el) (m el) el)) lst)
     lst))
  

(define (plain-manual-attribute-descriptions . par-list)
 (map
  (lambda (par)
   (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
   par-list))

(define (plain-manual-attribute-description attr-id attr-explanation)
  (list attr-id attr-explanation))
 


; ---------------------------------------------------------------------------------------------------
; MANUAL SECONDARY PLAIN STYLE:

(define light-red (make-color 255 100 100))
(define light-blue (make-color 150 150 255))

(define (secondary-manual-slice left-contribution right-contribution)
 ; return a horizontal slize of a manual contribution
 (table-1 0 '(12 140 12 "*")  (make-list 4 manual-background) 
      (list (list ""
                  (font-color light-red (b left-contribution))
                  ""
                  right-contribution)) 'top))

(define (plain-secondary-manual-title title-string)
  (con  (a-name title-string) 
        (h 2 (font-color light-blue (con title-string (horizontal-space 8) (secondary-element-indication))))))


(define (plain-secondary-manual-form form-list)
  (secondary-manual-slice "Form"
    (box (kbd (as-string form-list)))))

(define (plain-secondary-manual-pre-condition precon)
 (secondary-manual-slice 
    "Preconditions"
     (box precon)))

(define (plain-secondary-manual-description descr)
 (secondary-manual-slice 
   "Description"
   (box (capitalize-string-nd (strip-initial-spaces descr)))))

(define (plain-secondary-manual-parameters . par-list)
 (secondary-manual-slice "Parameters"
   (table-3 0 '(140 5 "*")
    (map
     (lambda (par)
      (present-element 
        'plain-secondary-manual-style
        (element-tag par)
        (element-contents par)))
    par-list) 'top)))


(define (plain-secondary-manual-parameter par explanation)
  (list  par ""    explanation) ; fed into a table row
)

(define (plain-secondary-manual-example ex)
 (secondary-manual-slice 
   "Example" (box (pre (font-size 2 ex)))))

(define (plain-secondary-manual-cross-references . ref-list)
 (secondary-manual-slice "See also"
   (table-3 0 '(140 5 "*")
    (map
     (lambda (ref)
      (present-element 
        'plain-secondary-manual-style
        (element-tag ref)
        (element-contents ref)))
    ref-list) 'top)))


(define (plain-secondary-manual-reference ref anchor url)
  (list ref ""  (a-tag url anchor)) ; fed into a table row
)

(define (plain-secondary-manual-internal-references ref . ref-list)
  (list ref ""  (string-concat (map (lambda (r) (con (a-tag (string-append "#" r) r) (horizontal-space 4))) ref-list)))
)

(define (plain-secondary-manual-comment comment)
 (secondary-manual-slice 
   "Internal remark" (box (em (font-size 2 comment)))))


(define (plain-secondary-manual-misc m)
 (secondary-manual-slice 
    "Note"
     (box m)))

(define (plain-secondary-manual-returns r)
 (secondary-manual-slice 
    "Returns"
     (box r)))

(define (plain-secondary-manual-section-title title-string)
  (let ((section-id (next-section-id)) ; increments next-section-number!
        (number next-section-number)
       )
    (con  (vertical-space 1)
          (a-name section-id)
          (b (font-color blue (con (font-size 6 (con (as-string number) "." (horizontal-space 3))) (font-rise (upcase-string title-string) 5)))))))

(define (plain-secondary-manual-section-body body)
  body)


(define (plain-secondary-manual-xml-in-laml-example-form f)
 (secondary-manual-slice 
    "Example form"
     (box (kbd (as-quoted-string-1 f)))))

(define (plain-secondary-manual-content-model cm)
 (secondary-manual-slice 
    "XML content model"
     (box (kbd cm))))

(define (plain-secondary-manual-attributes . par-list)
 (secondary-manual-slice "XML Attributes"
   (table-3 0 '(150 150 100)
    (map
     (lambda (par)
      (present-element 
        'plain-secondary-manual-style
        (element-tag par)
        (element-contents par)))
    par-list) 'top)))


(define (plain-secondary-manual-attribute attr-name attr-type attr-presence)
  (list  attr-name attr-type attr-presence) ; fed into a table row
)


(define (plain-secondary-manual-attribute-descriptions . par-list)
 (map
  (lambda (par)
   (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
   par-list))

(define (plain-secondary-manual-attribute-description attr-id attr-explanation)
  (list attr-id attr-explanation))




; ---------------------------------------------------------------------------------------------------
; HEADER PLAIN STYLE:


(define (header-manual-title title-string)
  (a-tag (string-append "#" title-string) title-string))

(define (header-manual-form form-list)
  (as-string form-list))

(define (header-manual-description descr)
 (capitalize-string-nd (strip-initial-spaces (manual-first-sentence-in-string descr))))

(define (header-manual-library library)
 (b library))

; ---------------------------------------------------------------------------------------------------
; TABLE OF CONTENTS STYLE:

(define (toc-manual-section-title title-string)
  title-string
 ; (a-tag (string-append "#" title-string) title-string)
 )


; ---------------------------------------------------------------------------------------------------

;;; Support of a master index. 
;;; A collection of indexes from several manual pages is called a master index.
;;; A master index is made manually from a separate LAML file which reads the saved .lsp files.
;;; The master index must be in the same directory as all the contributions.


;; Return the saved name.lsp manual info. Augment each entry with the library name and path information.
;; In that way we know for each manual entry where it comes from.
;; The second parameter is optional. If provided it gives the directory from which to read the lsp files.
;; The directory information is relative to the-library directory.
;; If it is not provided, read from the current directory.
(define (read-and-augment-lsp-file name . dir)
  (let ((dir-1 (if (not (null? dir)) (first dir) "")))
   (let ((res (file-read (string-append the-library dir-1 name ".lsp"))))
    (map 
      (lambda (entry) (cons (list 'path dir-1) (cons (list 'library name) entry)))
      res))))



;; The column widths of the master index. A list of 4 integers.
(define master-manual-index-width-list (list 140 70 300 400))

;; Return the master index given an augmented manual.
;; The augmentation is information about the library of each entry.
(define (manual-masterindex augmented-manual style)
  (table-1 1 master-manual-index-width-list (list white white white white)   
    (map
      masterindex-present-special
      (sort-list 
         (filter manual-page? augmented-manual)  ; disregard manual sections for the index
          manual-index-order-leq?))))

(define (masterindex-present-special entry)
  ; We use this special presentation because we need a non-local reference from the first column.
  ; Thus we do not use the nice style-based presentation, already prepared for the library entry
  ; this information cannot be made from the title alone.
  (let ((title (cadr (assq 'title entry)))
        (library (cadr (assq 'library entry)))
        (path (cadr (assq 'path entry)))
        (form (form-of-entry entry))
        (description (cadr (assq 'description entry))))
    (list (master-index-ref title library path) (b library) (as-string form) (manual-first-sentence-in-string description))))

; Return the form or xml-in-laml-example form of entry
(define (form-of-entry entry)
 (let ((form (get-defaulted-prop 'form entry #f))   ; TOO FEW PARAMETERS!
       (xml-in-laml-ex-form (get-defaulted-prop 'xml-in-laml-example-form entry #f))
      )
  (cond (form form)
        (xml-in-laml-ex-form xml-in-laml-ex-form)
        (else "??"))))  

(define (master-index-ref title library path)
  (a-tag (string-append path library ".html" "#" title) title))


;; Write a master index to the file manual-name.html.
;; The second parameter is optional. If provided it gives the directory of the resulting file. If not, write in the current directory.
;; A master index is a normal index, but in addition it contains a libray column
;; Normally the parameters all-entries are made by mapping read-and-augment-lsp-file over the saved lsp files with
;; manual information. Next these lists are append accumulated.
(define (make-master-index all-entries . dir)
  (let ((dir-1 (if (not (null? dir)) (first dir) "")))
   (write-text-file
    (page
       manual-title
       (con
        (left-middle-right-banner (when-generated) (string-append "Copyright " copyright " 2000, Kurt Nørmark") (laml-home-button 0 "laml-home.gif"))
        (font-color blue (h 1 manual-title)) (p) 
        (present-author-info manual-author) (p)
        (if (and #f home-url) (con (a-tag home-url "Home") (p)) "")
        (indent-pixels 12 
           (con (em "Alphabetic index:") 
           (manual-masterindex all-entries 'header-manual-style))) (p)
        (font 2 red (when-generated)) (br)
        (font 2 red end-remark)
       ))
     (string-append dir-1 (as-string manual-name) ".html"))))


;; Old
(define (manual-table-of-contents manual style)
 (let ((sections (filter manual-section? manual)))
  (ol
    (map2
      (lambda (manual-section section-number) 
        (let ((section-text  (car (format-manual-page manual-section style))))
          (a-tag (string-append "#" (section-id section-number)) section-text))
      )
      sections
      (number-interval 1 (length sections))))))


(define (manual-table-of-contents manual style)
 (let ((sections (filter manual-section? manual))
       (toc-width (accumulate-right + 0 manual-index-width-list)))
  (n-column-list 3
    (map2
      (lambda (manual-section section-number) 
        (let ((section-text  (car (format-manual-page manual-section style))))
          (a-tag
            (string-append "#" (section-id section-number))
            (font-size 2 (con (as-string section-number) ". " section-text))))
      )
      sections
      (number-interval 1 (length sections)))
    toc-width )))

(define (number-of-sections? manual)
  (length (filter manual-section? manual)))

; ---------------------------------------------------------------------------------------------------
; SECTION ID ADMINISTRATION

(define next-section-number 0)

; Return the next section id.
(define (next-section-id)
  (set! next-section-number (+ next-section-number 1))
  (string-append "SECTION" (as-string next-section-number)))

; Return section id of section number (an integer)
(define (section-id number)
  (string-append "SECTION" (as-string number)))

; ---------------------------------------------------------------------------------------------------
;;; XML-in-LAML support. 
;;; Extraction of manual stuff from a parsed dtd and merging of manual pages.

; DTD selector functions

(define tag-dtd (make-selector-function 1 "tag-dtd"))
(define element-name-dtd (make-selector-function 2 "element-name-dtd"))
(define element-content-model-dtd (make-selector-function 5 "element-content-model-dtd"))
(define attribute-name-dtd (make-selector-function 2 "attribute-name-dtd"))
(define attribute-list-dtd (make-selector-function 3 "attribute-list-dtd"))

;; Return a manual list, as extracted from a parsed dtd.
;; .parameter dtd-list-structure the Lisp structure, as generated by the LAML DTD parser.
(define (manual-from-parsed-dtd dtd-list-structure)
 (let* ((elements   (filter (lambda (x) (eq? 'element (tag-dtd x))) dtd-list-structure))
        (attributes (filter (lambda (x) (eq? 'attribute (tag-dtd x))) dtd-list-structure))
        (sorted-attributes		; attributes sorted in the same order as elements
	 (sort-list
	  attributes
	  (generate-leq (map (compose as-symbol cadr) elements) (compose as-symbol cadr)))))
  (map2 (lambda (el attr) (manual-entry-from-parsed-dtd el attr)) elements sorted-attributes)))

(define (manual-entry-from-parsed-dtd el attr)
 (let ((title-contr (element-name-dtd el))
       (content-contr (element-content-model-dtd el))
       (attr-list-contr (attribute-list-dtd attr))
       (attribute-maker (lambda (alc) (cons 'attribute (eliminate-fixed-component alc))))
      )
   (list 
     (list 'kind "manual-page")
     (list 'title title-contr)
     (list 'content-model (present-content-model content-contr))
     (cons 'attributes (map attribute-maker attr-list-contr)))))

; Transform elements such as 
; (name ("preserve") "preserve" "#FIXED" "-") to
; (name ("preserve") "preserve" "-").
; Keep element such as 
; (name y #IMPLIED -)
; unchanged.
(define (eliminate-fixed-component attribute-structure)
  (if (and (= (length attribute-structure) 4) (equal? (as-string (fourth attribute-structure)) "#FIXED"))
      (front-sublist attribute-structure 3)
      attribute-structure))

; Unparsing of the parsed content model, as referred by content-contr.
; (define (present-content-model content-contr)
;  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
;         (lenient-string<=? (lambda (x y) (string<=? (as-string x) (as-string y))))
;         (sorter (lambda (lst) (sort-list lst lenient-string<=?)))
;        )
;   (cond ((string? content-contr) content-contr)
; 	((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'sequence-with-optionals))
; 	 (string-append 
; 	  "(" 
; 	  (list-to-string (map (lambda (x) (if (symbol? x) (string-append (mr (as-string x)) "?") (mr x))) (cdr content-contr)) ",")
; 	  ")" ))
; 
; 	((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'one-or-more))
; 	 (string-append 
; 	  "(" 
; 	  (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
; 	  ")+"))
; 
; 	((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'zero-or-more))
; 	 (string-append 
; 	  "(" 
; 	  (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
; 	  ")*"))
; 
; 	((and (symbol? content-contr) (eq? 'pcdata-checker content-contr)) "(#PCDATA)")
; 
; 	((symbol? content-contr) (as-string content-contr))
; 
; 	(else (laml-error "present-content-model: unknown kind of content model:" content-contr)))))

; ---------------------------------------------------------------------------------------------------
; Content model selectors. 
; Duplicate code from tools/xml-in-laml/xml-in-laml.scm

(define (empty-element-rhs? parsed-content-model)
  (and (symbol? parsed-content-model) (eq? parsed-content-model 'empty)))

(define (any-element-rhs? parsed-content-model)
  (and (symbol? parsed-content-model) (eq? parsed-content-model 'any)))

(define (mixed-content-rhs? parsed-content-model) ; (mixed-content pcdata)
  (and (list? parsed-content-model) (> (length parsed-content-model) 1)
       (eq? (car parsed-content-model) 'mixed-content)))

(define (mixed-content-pcdata-only-rhs? parsed-content-model) ; THE example: (mixed-content pcdata)
  (and (mixed-content-rhs? parsed-content-model) (= (length parsed-content-model) 2)
       (eq? (cadr parsed-content-model) 'pcdata)))

(define (element-content-rhs? parsed-content-model) ; (mixed-content pcdata)          ; an example: (element-content (seq one (name one "head") (name one "body")))
  (and (list? parsed-content-model) (> (length parsed-content-model) 1)
       (eq? (car parsed-content-model) 'element-content)))

; ---------------------------------------------------------------------------------------------------

; Unparsing of the parsed content model, as referred by content-contr.
; As of March 2003.
(define (present-content-model content-contr)
  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
         (lenient-string<=? (lambda (x y) (string<=? (as-string x) (as-string y))))
         (sorter (lambda (lst) (sort-list lst lenient-string<=?)))
        )
   (cond ((empty-element-rhs? content-contr) "EMPTY")
	 ((any-element-rhs? content-contr) "ANY")
	 ((mixed-content-pcdata-only-rhs? content-contr) "(#PCDATA)")
	 ((mixed-content-rhs? content-contr) 
          (let ((choice-list (cddr (cadr content-contr))))  ; cddr: take #PCDATA for granted.
            (string-append 
	     "(#PCDATA |"
             (list-to-string  (map mr (sorter choice-list)) " | ")
	     ")*")))
	 ((element-content-rhs? content-contr)
           (let ((content-substance (cadr content-contr)))
             (present-element-content content-substance)))
	 (else (laml-error "present-content-model: Unknown form of parsed content model" content-contr)))))

; Recursive unparsing of element-content-forms
(define (present-element-content content-substance)
  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
        )
   (let ((kind (car content-substance))
	 (mult (cadr content-substance))
	 (body-list (cddr content-substance))
	 (mult-unparse (lambda (mult) 
			 (cond ((eq? mult 'one) "")
			       ((eq? mult 'optional) "?")
			       ((eq? mult 'zero-or-more) "*")
			       ((eq? mult 'one-or-more) "+")
			       (else (laml-error "mult-unparse: Unknown kind of multiplicity symbol" mult)))))
	 )
     (cond ((eq? kind 'name)
	    (let ((name (car body-list)))
	      (string-append (mr (as-string name)) (mult-unparse mult))))
	   ((eq? kind 'seq)
	    (string-append "(" (list-to-string (map present-element-content body-list) ",") ")" (mult-unparse mult)))
	   ((eq? kind 'choice)
	    (string-append "(" (list-to-string (map present-element-content body-list) " | ") ")" (mult-unparse mult)))
	   ((eq? kind 'empty)	     ; does not appear in parsed forms
	    "")))))

; ---------------------------------------------------------------------------------------------------


;; Merge the manual page lists man-list-1 and man-list-2.
;; Appart from title, the fields in the two manual lists should be disjoint.
;; The first one controls the ordering of the resulting list.
;; Entries of man-list-2 which do not have a counterpart in man-list-1 are not included in the result.
(define (merge-manual-pages man-list-1 man-list-2)
  (map (lambda (mp1) (merge-manual-page mp1 man-list-2)) man-list-1))

(define (merge-manual-page mp1 man-list-2)
  (let ((other-mp 
	 (find-in-list
	  (lambda (mp2) 
	    (let ((mp2-title (assq 'title mp2))
		  (mp1-title (assq 'title mp1)))
	      (if (and mp1-title mp2-title)
		  (equal? (cadr mp2-title) (cadr mp1-title))
		  #f)))
	  man-list-2))
	(remove-title-from (lambda (mp-lst) (filter (lambda (e) (not (eq? 'title (car e)))) mp-lst)))
       )
    (if other-mp 
        (append mp1 (remove-title-from other-mp))
        mp1)))


; Return a function which extends a manual-entry with field (symbol) and value.
(define (manual-extend field value)
  (lambda (manual-entry)
    (cons (list field value) manual-entry)))

; ---------------------------------------------------------------------------------------------------
        

; Get property key from the property list.
; In this context a property list is a structure like ((k1 v1) (k2 v2) ... ),
; thus in reality an association list with (k v) instead of (k . v) entries.
(define (get-prop key prop-list)
 (let ((res-list (get key prop-list)))
   (if (and res-list (pair? res-list))
       (car res-list)
       (laml-error "get-prop: not possible to extract" key))))

; A defaulted variant of get-prop
(define (get-defaulted-prop key prop-list default)
 (let ((res-list (defaulted-get key prop-list default)))
   (if (and res-list (pair? res-list))
       (car res-list)
       default)))

