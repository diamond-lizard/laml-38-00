; LAML Manual page style.
; The essential XHTML backend processing with CSS stylesheets, which is used from both the original manual style
; and the XML-in-LAML manual style.

; COPYRIGHT (c)  Kurt Nørmark, Department of Computer Science, Aalborg University, Denmark. 2003.
; normark@cs.auc.dk,  http://www.cs.auc.dk/~normark/


; ---------------------------------------------------------------------------------------------------
; Make a more sparse html char transformation table for xhtml in manuals.
; This table does not transliterate '<' and '>'

(define manual-html-char-transformation-table
    (list->vector (make-list 256 #t)))

(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\") "&quot;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\') "&#39;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\&) "&amp;")

(set-xml-char-transformation-table-in 'xhtml10-transitional manual-html-char-transformation-table)

; In order to preserve white space in textual pre elements from manual entries and manual section entries:
(set! xml-always-render-white-space? #t)

; ---------------------------------------------------------------------------------------------------
; Constants

; Do we want the manual style to display information while it processes a manual.
; 0 means silence. 1 means print some info. 
(define manual-verbosity-level 0)

(define schemedoc-url "http://www.cs.auc.dk/~normark/schemedoc/")

(define max-section-number #f) ; assigned by manual-page

; The absolute destination path to the directory with HTML files etc. 
; Re-initialized by context styles/xml-in-laml/manual/manual.scm
; Also re-initialized by make-manual, for the sake of cases where make-manual is called without the xml-in-laml stuff.
(define the-manual-destination-path (startup-directory))

; The kind of sorting applied on attributes.
; Either as-in-dtd (default) or by-attribute-name
(define attribute-sorting-kind 'as-in-dtd) 

; A boolean variable that controls the creation of links to HTML, elucidative programming style source programs   
(define link-to-ep-source-program? #f)

; Corresponds to EP the manual source list. Only used if the variable link-to-ep-source-program? is true.
(define scheme-source-linking-manual-list '())


; ---------------------------------------------------------------------------------------------------
; A variation of as-quoted-string from the library general.scm which treats subexpressions of the
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

; Meta information about the current manual. Written as the first element in the internal -.manlsp file
(define manual-meta-information '())

; CSS stylesheet management.
; CSS stylesheets can be defined in the stylesheets directory of the manual source directory
; or in the stylesheets directory of the manual software directory. Exactly as in LENO.
; An aggregation of the pre css stylesheet and the css stylesheet from these two directories
; is copied to the destination directory. The destination directory dependes on the the variable
; css-stylesheet-schema.

; Where to find the manual css stylesheet. A symbol.
; laml: Use manual-stylesheet.css in the css-stylesheets/manual directory of the laml root.
; local: Use the local css file of the same proper name as the manual source file.
(define css-stylesheet-schema 'laml)

; Copy a css stylesheet from the stylesheet directory of the manual software source directory
; to the location determined by the variable css-stylesheet-schema. 
; If #f, no copying is done.
(define copy-css-stylesheet? #t)

; The name of the css stylesheet, as present in the source stylesheets directory. String without file file extension.
; If copy-css-stylesheet?, this stylesheet is copied to the target stylesheet location.
(define the-manual-stylesheet "original")

; The name of the pre css stylesheet which is put in front of the the-manual-stylesheet.
(define the-manual-prestylesheet "compact")

; Return the relative path from the startup directory to the css manual stylesheet.
; Destination dir is normalized (precondition).
; The parameter style-sheet-schema is supposed to be the constant css-stylesheet-schema.
(define (css-stylesheet-path stylesheet-schema destination-dir)
  (cond ((eq? stylesheet-schema 'laml)
           (string-append (laml-dir-prefix destination-dir) "css-stylesheets/manual/" "manual-stylesheet.css"))
        ((eq? stylesheet-schema 'local)
         (string-append manual-name "." "css"))
        (else (laml-error "css-stylesheet-path: Unknown style-sheet schema" stylesheet-schema))))

(define (read-text-file-if-exists file-path)
  (if (file-exists? file-path)
      (read-text-file file-path)
      ""))

(define (copy-css-stylesheet! absolute-destination-dir)
  (let ((manual-source-css-pre-filepath (string-append manual-source-directory "stylesheets/" the-manual-prestylesheet ".css"))
        (manual-software-css-pre-filepath  (string-append manual-software-directory "stylesheets/" the-manual-prestylesheet ".css"))
        (manual-source-css-filepath (string-append manual-source-directory "stylesheets/" the-manual-stylesheet ".css"))
        (manual-software-css-filepath  (string-append manual-software-directory "stylesheets/" the-manual-stylesheet ".css"))
        (manual-target-css-filepath 
             (cond ((eq? css-stylesheet-schema 'laml)
                    (string-append laml-dir "css-stylesheets/manual/" "manual-stylesheet.css"))
                   ((eq? css-stylesheet-schema 'local)
                    (string-append absolute-destination-dir manual-name "." "css"))
                   (else (laml-error "css-stylesheet-path: Unknown style-sheet schema" css-stylesheet-schema))))
       )
    (write-text-file
     (string-append
      (read-text-file-if-exists manual-software-css-pre-filepath) CR CR (read-text-file-if-exists manual-source-css-pre-filepath) CR CR
      (read-text-file-if-exists manual-software-css-filepath) CR CR (read-text-file-if-exists manual-source-css-filepath)
      )
     manual-target-css-filepath
     )

    (if (and (not (file-exists? manual-source-css-pre-filepath))
             (not (file-exists? manual-software-css-pre-filepath)))
        (display-warning "Cannot locate any CSS prestylesheet named" the-manual-prestylesheet))

    (if (and (not (file-exists? manual-source-css-filepath))
             (not (file-exists? manual-software-css-filepath)))
        (display-warning "Cannot locate any CSS stylesheet named" the-manual-stylesheet))
    ))

; ---------------------------------------------------------------------------------------------------
; MAINLY PREAMBLE FUNCTION DEFINITIONS

(define laml-source-file-name-without-extension (source-filename-without-extension))

; The explanation of attributes, for which no attribute-description is available
(define standard-attribute-explanation "-")

(define the-manual-title "")
(define the-manual-author '())
(define the-manual-abstract "")
(define home-url #f)
(define manual-master-index #f)

(define manual-name (if (not (equal? laml-source-file-name-without-extension ""))
                        laml-source-file-name-without-extension
                        "manual"))


(define manual-page-list '()) ; the list in which we collect manual page descriptions

(define manual-background white)

(define language-preference 'english) ; redefinition

(define laml-manual-stuff #t)   ; true if making a manual for laml-stuff. Affects the header of the generated html file.



;;; Programmatic support of manual generation. 
;;; The functions in the following are selected functions from the style
;;; manual.scm, which are useful when manuals are made from other programs.


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


(define manual-first-sentence-in-string first-sentence-in-string)

; return a reference to another a-named place in the current document
(define (internal-ref anchor)
  (a-tag (this-reference anchor) anchor))

(define (this-reference ref)
  (string-append "#" ref))



; the mutual ordering among manual tags:
(define manual-element-order 
  '(section-id section-title section-body title library kind form xml-in-laml-example-form description content-model attributes 
    pre-condition parameters xml-in-laml-attributes returns post-condition example cross-references misc comment))

; the list of possible and supported top level elements of a manual page. Manual section entries are not included in this list.
(define supported-manual-page-elements 
  '(title library kind form description pre-condition post-condition parameters example cross-references misc comment returns
          xml-in-laml-example-form attributes content-model level xml-in-laml-attributes ))  ; add attribute-descriptions - and bring plain-manual-attribute-descriptions up to date!

; the list of possible and supported top level elements of a manual section. Manual page entries are not included in this list.
(define supported-manual-section-elements
  '(kind section-id section-title section-body level))

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
              ((eq? kind 'post-condition) plain-manual-post-condition)
              ((eq? kind 'description) plain-manual-description)
              ((eq? kind 'parameters) plain-manual-parameters)
              ((eq? kind 'parameter) plain-manual-parameter)  ; not used
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
              ((eq? kind 'section-id) plain-manual-section-id)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) plain-manual-xml-in-laml-example-form)   ; information from XML DTDs
              ((eq? kind 'content-model) plain-manual-content-model)
              ((eq? kind 'attributes) plain-manual-attributes)
              ((eq? kind 'attribute) plain-manual-attribute)

              ((eq? kind 'attribute-descriptions) plain-manual-attribute-descriptions)   ; manually authored information - does it work?
              ((eq? kind 'attribute-description) plain-manual-attribute-description)   

              ((eq? kind 'xml-in-laml-attributes) plain-manual-xml-in-laml-attributes)   ; Extracted from Scheme program - documentation of XML-in-LAML ad hoc abstractions
              ; ((eq? kind 'xml-in-laml-attribute)  plain-manual-xml-in-laml-attribute)   

              ((eq? kind 'level) empty-element)

              (else (laml-error  (as-string kind) ": " "Cannot find tag in plain-manual-style"))))

       ((eq? style 'plain-manual-style-level-2)                        ; only the title field is special
        (cond ((eq? kind 'title) plain-manual-title-level-2)
              ((eq? kind 'section-title) plain-manual-section-title-level-2)
              (else (find-processor 'plain-manual-style kind))))


       ((eq? style 'header-manual-style)
        (cond ((eq? kind 'title) header-manual-title)
              ((eq? kind 'form) header-manual-form)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
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
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) header-manual-library)

              ((eq? kind 'xml-in-laml-example-form) header-manual-form)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              ((eq? kind 'level) empty-element)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in header-manual-style")))))

       ((eq? style 'header-manual-style-xml)              ; Headers for manuals that stem from XML DTDs
        (cond ((eq? kind 'title) header-manual-title)
              ((eq? kind 'form) empty-element)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
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
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) header-manual-library)

              ((eq? kind 'xml-in-laml-example-form) empty-element)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              ((eq? kind 'level) empty-element)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in header-manual-style-xml")))))

       ((eq? style 'toc-manual-style)
        (cond ((eq? kind 'title) empty-element)
              ((eq? kind 'form) empty-element)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
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
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) empty-element)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'level) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              (else (error (string-append (as-string kind) ": " "Cannot find tag in toc-manual-style")))))

       (else (error "Unknown style"))))

; In an AST based XHTML representation it comes in handy to represent the empty element as the explicit white space
(define (empty-element . pars)
  explicit-space)

(define (empty-manual-page-element? x)
  (forced-white-space? x))


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

; Primary/secondary element indications to be shown after the title.
; Can be redefined in laml files that produce concrete manuals.
(define (primary-element-indication) "")
(define (secondary-element-indication) "")


; ---------------------------------------------------------------------------------------------------
; CENTRAL PRESENTATION FUNCTIONS:

; A variable in which we collect given section-id in manual-section clauses
(define explicit-section-id "")

; Returns a list of html-markup.
; The parameter elements (a list) represents of a single manual page or a single manual-section.
(define (format-manual-page elements primary-style-level-1 primary-style-level-2 . optional-parameter-list)
 (set! explicit-section-id "")
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style-level-1))
       (the-level (get-defaulted-prop 'level elements 1))
      )
  (cond ((and (manual-section? elements) (= the-level 1))
           (map
            (lambda (e)
              (present-element
               (if (primary-element? elements) primary-style-level-1 secondary-style)
               (element-tag e)
               (element-contents e)) 
              )
            (sort-list elements manual-element-ordering))
        )
        ((and (manual-section? elements) (= the-level 2))
           (map
            (lambda (e)
              (present-element
               (if (primary-element? elements) primary-style-level-2 secondary-style)  
               (element-tag e)
               (element-contents e)) 
              )
            (sort-list elements manual-element-ordering))

        )
        ((and (manual-page? elements) (= the-level 1)) 
          (filter (compose not empty-manual-page-element?) 
            (map
             (lambda (e)
               (present-element
                (if (primary-element? elements) primary-style-level-1 secondary-style)
                (element-tag e)
                (element-contents e)) 
               )
             (sort-list elements manual-element-ordering))
            ))
        ((and (manual-page? elements) (= the-level 2)) 
          (filter (compose not empty-manual-page-element?) 
            (map
             (lambda (e)
               (present-element
                (if (primary-element? elements) primary-style-level-2 secondary-style)
                (element-tag e)
                (element-contents e)) 
               )
             (sort-list elements manual-element-ordering))
            ))
        (else  (laml-error "format-manual-page: unknown manual entry" elements primary-style-level-1 primary-style-level-2)))))


(define (end-manual-entry)
  (tr  (td 'class (manual-plain-css-class "end-of-entry") 'colspan "4" (hr 'class (manual-plain-css-class "end-of-entry")))))

(define (line-breaker str)
  (con str (br)))

; title info, for processing feedback.
(define (info-title manual-page)
 (let ((ttl1 (assq 'title manual-page))
       (ttl2 (assq 'section-title manual-page)))
  (cond (ttl1 (cadr ttl1))
        (ttl2 (cadr ttl2))
        (else "????"))))

; manual is a list, such as manual-page-list
; thus manual is the whole manual struture.
(define (format-manual manual primary-style-level-1 primary-style-level-2  . optional-parameter-list)
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style-level-1)))
   (html:table 'class "manual-table"
    (map
      (lambda (manual-page)
        (if (>= manual-verbosity-level 1) (display-message (info-title manual-page)))
        (append
          (format-manual-page
               manual-page primary-style-level-1 primary-style-level-2 secondary-style)
          (list (end-manual-entry)))
      )
      (filter page-to-be-presented? manual)))))

(define (page-to-be-presented? manual-page)
  (let ((the-level (get-defaulted-prop 'level manual-page 1)))
    (<= the-level 2))) 

(define (present-author-info au)
 (let ((au1 (if (not (null? au)) (cons (copyright-owner (car au)) (cdr au)) au)))
   (h3 
     (map (lambda (au-el) (span 'class "manual-author-element" au-el)) au1))))


(define (present-abstract abstr)
 (div 'class "manual-abstract" abstr))



;; This function makes the manual from collected information.
;; In this sense, this is the function which makes things happen.
;; This functions reverses the order of entries.
;; Take a look at one of the lsp files with manual information in order
;; to find out about the list structures of a manual-list.
;; A HTML file is written containing the formatted manual.
;; The HTML file is written to a particular destination-dirctory
;; (if this information is provided as the second parameter), else to the current directory.
;; .form (make-manual [manual-pages manual-kind destination-dir source-file-name-information])
;; .parameter manual-pages The list of manual pages and manual sections of this manual. Defaults to the value of the variable manual-page-list.
;; .parameter manual-kind The kind of manual, manual-pages-sections-only, manual-from-scheme-file, manual-from-xml-dtd, or merged-manual-from-xml-dtd. Defaults to manual-from-scheme-file. A symbol.
;; .parameter destination-dir The absolute destination directory of the HTML file. Defaults to the value of destination-path.
;; .parameter source-file-name-information The name of the source file, which optionally can be supplied for the purpose of documentation.
(define (make-manual . par-list)
 (let* ((manual-page-list-1 (if (null? par-list) 
                                (reverse manual-page-list)  ; the global variable
                                (reverse (first par-list))  ; the first parameter - par-list is bound to a list of passed parameters...
                            ))
        (manual-kind (if (>= (length par-list) 2) (second par-list) 'manual-from-scheme-file))
        (manual-page-list-2 (merge-attributes-and-descriptions manual-page-list-1)) ; join attributes and attribute-descriptions
                                                                                    ; by adding attribute description as an extra element
                                                                                    ; to an attribute. Attribute-descriptions stay
                                                                                    ; as non-presented informations in manual-page-list-2


        (manual-page-list-3 (map clean-manual-page manual-page-list-2))   ; remove unkwown fields from manual pages

        (destination-dir (if (>= (length par-list) 3) (third par-list) the-manual-destination-path))
        (source-file-name-information (if (>= (length par-list) 4) (fourth par-list) ""))
        (table-of-contents? (> (number-of-sections? manual-page-list-3) 0))
        (yr (car (time-decode (current-time))))
       )

  (set! the-manual-destination-path destination-dir)
  (set! max-section-number (length (filter manual-section? manual-page-list-3)))

  (if (not laml-manual-stuff)   ; copy images to a local man-images dir
      (begin
        (ensure-directory-existence! destination-dir "man-images")
        (copy-files 
         (list "small-next-blind.gif" "small-next.gif" "small-prev-blind.gif" "small-prev.gif" "small-up-blind.gif" "small-up.gif")
         (string-append manual-software-directory "images/")
         (string-append destination-dir "man-images/"))))

  (write-html '(raw prolog)
   (html
     (head (html:title the-manual-title)
           (link 'href 
                 (css-stylesheet-path css-stylesheet-schema destination-dir)
                 'rel "stylesheet" 'type "text/css")
           (if laml-manual-stuff (link 'rel "SHORTCUT ICON" 'href (string-append (laml-home-url-prefix 0 the-manual-destination-path) "images/16-16-icon.ico")) '())
     )
     (body 'class "manual-body"
        (a-name "MANUAL-TOP")
        (if laml-manual-stuff 
            (left-middle-right-banner (when-generated) (span "Copyright " copyright  (as-string yr) ", Kurt Nørmark")
                                      (laml-home-button 0 "laml-home.gif" destination-dir))

            (left-middle-right-banner (em (when-generated)) "" (em "A" (a 'href schemedoc-url "SchemeDoc") "Manual") ))
            
        (h1 'class "manual-title" the-manual-title) (p) 
        (present-author-info the-manual-author) (p)
        (if (and #f home-url (not (equal? home-url ""))) (con (a-tag home-url "Home") (space 3)) "")   ; no home page ref any more
        
        (if manual-master-index 
            (con (a-tag manual-master-index "Master index") (br))
            "")

        (if (not (empty-string? source-file-name-information))
            (font-color grey (con "Source file: " source-file-name-information (br)))
            "")
        (font-color grey (con "LAML " laml-version))
  
        (p)

        (a-name "MANUAL-ABSTRACT")
        (present-abstract the-manual-abstract) (p)


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
                    (manual-index manual-page-list-3 
                                  (if (or (eq? manual-kind 'manual-from-xml-dtd) (eq? manual-kind 'merged-manual-from-xml-dtd)) 
                                      'header-manual-style-xml
                                      'header-manual-style))
            )) (p))
           "")

        
        (format-manual manual-page-list-3 'plain-manual-style 'plain-manual-style-level-2 'plain-secondary-manual-style)
        (p) (hr) 
        (font-1 2 red (when-generated)) (br)
        (font-1 2 red (span "Generated by LAML" (a 'href schemedoc-url (font-color red "SchemeDoc")) _ ".")) (br)
        (font-1 2 red end-remark)
        (vertical-space 25)  ; to allow for scrolling at the end of a manual page
      )
     )
   (string-append destination-dir (as-string manual-name) ".html"))

   ; Write the uncleaned manual page to a -.lsp file. This keeps specialized fields for other purposes.
   (if (<= (length par-list) 1)
       (write-lisp-form-manual manual-page-list-1)
       (write-lisp-form-manual manual-page-list-1 manual-source-directory))

   (if copy-css-stylesheet?
       (copy-css-stylesheet! destination-dir))

 )
)


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
 (cond ((manual-page? page)
           (filter 
            (lambda (element) (memq (car element) supported-manual-page-elements)) 
            page))
       ((manual-section? page)
           (filter 
            (lambda (element) (memq (car element) supported-manual-section-elements)) 
            page))
       (else (laml-error "clean-manual-page: Unknown kind of page:" page))))


    

;; A possible end remark added as the last line of the documentation in small font. Default is the empty string.
(define end-remark "") 

; return a horizontal slize of a manual contribution
; As an optional parameter this function accept an explanation of the attribute
(define (manual-slice left-contribution right-contribution . optional-parameter-list)
 (tr (td "-")))

; ---------------------------------------------------------------------------------------------------
;;; Index functions. 
;;; The functions supporting the generation of a manual index.

;; The widths of the colums of the manual index.  A list of three integers
(define manual-index-width-list (list 140 300 400))

(define (manual-index manual style)
  (html:table 'border "3"
    (map
      (lambda (manual-page)
         (tr (format-manual-page (only-one-form manual-page) style style)))
      (sort-list 
         (filter manual-page? (filter page-to-be-presented? manual))  ; disregard manual sections for the index
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
;; The filename is manual-name.manlsp.
;; If the optional parameter dir is given write the manlsp file in this directory.
;; Else write it in the manual source directory.
;; If provided, dir ends in a slash. 
;; The lsp file is useful for programmatic access to the entire body of manual information.
(define (write-lisp-form-manual manual-page-list . optional-parameter-list)
 (let ((dir (optional-parameter 1 optional-parameter-list manual-source-directory)))
  (let* ((lisp-form-manual-file-name (string-append dir (as-string manual-name) ".manlsp"))
         (extended-manual-page-list (append (list manual-meta-information) manual-page-list))
        )
    (if (file-exists? lisp-form-manual-file-name)
        (delete-file lisp-form-manual-file-name))

    (with-output-to-file lisp-form-manual-file-name
                         (lambda () (write  extended-manual-page-list))))))



; ---------------------------------------------------------------------------------------------------
; MANUAL PLAIN STYLE:

(define (plain-manual-title title-string)
  (manual-slice-1  
     (manual-plain-css-class "title")
     (span (a 'name title-string) title-string)))

(define (plain-manual-title-level-2 title-string)
  (manual-slice-1  
     (manual-plain-css-class "title-level-2")
     (span (a 'name title-string) title-string)))


(define (plain-manual-form form-list)
  (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Form"
    (manual-plain-css-class "form") (as-string form-list)))

(define (plain-manual-pre-condition precon)
 (manual-slice-2  (manual-plain-css-class "entry-kind") "Precondition" (manual-plain-css-class "condition") precon))

(define (plain-manual-post-condition postcon)
 (manual-slice-2  (manual-plain-css-class "entry-kind") "Postcondition" (manual-plain-css-class "condition") postcon))


(define (plain-manual-description descr)
 (manual-slice-2
   (manual-plain-css-class "entry-kind") "Description"
   (manual-plain-css-class "description")
   (let ((description (strip-initial-spaces (xml-render-as-xml descr))))
     (if (empty-string? description) "" (capitalize-string-nd description)))))   ; Notice the rendering

(define (plain-manual-parameters . par-list)
 (if (not (null? par-list))
     (let ((first-par (car par-list))
           (rest-pars (cdr par-list)))
       (cons
         (manual-slice-3-rowspan (length par-list) 
           (manual-plain-css-class "entry-kind") "Parameters"
           (manual-plain-css-class "parameter-name") (manual-parameter-name first-par)
           (manual-plain-css-class "parameter-description") (manual-parameter-description first-par))
         (map 
           (lambda (par)
             (manual-slice-2 
              (manual-plain-css-class "parameter-name") (manual-parameter-name par)
              (manual-plain-css-class "parameter-description") (manual-parameter-description par)))
           rest-pars)))
     '()))

(define manual-parameter-name (make-selector-function 2 "manual-parameter-name"))
(define manual-parameter-description (make-selector-function 3 "manual-parameter-description"))

; Is intended to handle manually authored attribute description, which are not merged with XML DTD info.
; Probably not used ???
(define (plain-manual-attribute-descriptions . par-list)
 (map
  (lambda (par)
   (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
   par-list))

; Is intended to handle manually authored attribute description, which are not merged with XML DTD info.
; Probably not used ???
(define (plain-manual-attribute-description attr-id attr-explanation)
  (list attr-id attr-explanation))


; Handles attributes of ad hoc XML-in-LAML abstractions. 
(define (plain-manual-xml-in-laml-attributes . attr-list)
 (if (not (null? attr-list))
     (let ((first-attr (car attr-list))
           (rest-attrs (cdr attr-list))
           (presence-indication (lambda (status)    
                                   (if (equal? status "required") (font-color red (b " *")) "")))
          )
       (cons
         (manual-slice-3-rowspan (length attr-list) 
           (manual-plain-css-class "entry-kind") "Attributes"
           (manual-plain-css-class "attribute-name") (con (manual-xml-in-laml-attribute-name first-attr) (presence-indication (manual-xml-in-laml-attribute-status first-attr)))
           (manual-plain-css-class "attribute-description") (manual-xml-in-laml-attribute-description first-attr)
           (con "Required: " (font-color red (b "*")) )
         )
         (map 
           (lambda (attr)
             (manual-slice-2 
              (manual-plain-css-class "attribute-name") (con (manual-xml-in-laml-attribute-name attr) (presence-indication (manual-xml-in-laml-attribute-status attr)))
              (manual-plain-css-class "attribute-description") (manual-xml-in-laml-attribute-description attr)))
           rest-attrs)))
     '()))

(define manual-xml-in-laml-attribute-name (make-selector-function 2 "manual-xml-in-laml-attribute-name"))
(define manual-xml-in-laml-attribute-status (make-selector-function 3 "manual-xml-in-laml-attribute-status"))
(define manual-xml-in-laml-attribute-description (make-selector-function 4 "manual-xml-in-laml-attribute-description"))

(define (plain-manual-example ex)
 (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Example"
    (manual-plain-css-class "example") (pre ex)))

(define (plain-manual-cross-references . ref-list)
  (if (not (null? ref-list))
      (let ((first-ref (car ref-list))
            (rest-refs (cdr ref-list)))
        (cons
          (manual-slice-3-rowspan (length ref-list) 
            (manual-plain-css-class "entry-kind") "See also"
            (manual-plain-css-class "reference-role") (manual-reference-role first-ref)
            (manual-plain-css-class "reference-anchor") 
            (if (eq? (manual-reference-kind first-ref) 'reference)
                (a 'class (manual-plain-css-class "external-reference") 
                   'href (manual-reference-url first-ref) (manual-reference-anchor first-ref))
                (separate-list 
                   (map (lambda (r) (a 'class (manual-plain-css-class "internal-reference")
                                       'href (string-append "#" r) r)) (manual-internal-references first-ref)) "")))
                   
          (map 
            (lambda (ref)
              (manual-slice-2 
                (manual-plain-css-class "reference-role") (manual-reference-role ref)
                (manual-plain-css-class "reference-anchor") 
                (if (eq? (manual-reference-kind ref) 'reference)
                    (a 'class (manual-plain-css-class "external-reference")
                       'href (manual-reference-url ref) (manual-reference-anchor ref))
                    (separate-list 
                      (map (lambda (r) (a 'class (manual-plain-css-class "internal-reference")
                                          'href (string-append "#" r) r)) (manual-internal-references ref)) ""))))  
            rest-refs))
       )
      '()))

(define manual-reference-kind (make-selector-function 1 "manual-reference-kind"))
(define manual-reference-role (make-selector-function 2 "manual-reference-role"))

(define manual-reference-anchor (make-selector-function 3 "manual-reference-anchor"))
(define manual-reference-url (make-selector-function 4 "manual-reference-url"))

(define manual-internal-references cddr)


(define (plain-manual-comment comment)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Internal remark"
    (manual-plain-css-class "comment") comment))

(define (plain-manual-misc m)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Note"
    (manual-plain-css-class "misc") m))

(define (plain-manual-returns r)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Returns"
    (manual-plain-css-class "returns") r))


(define (plain-manual-section-title title-string)
  (let ((section-id (next-section-id)) ; increments next-section-number!
        (number next-section-number)
       )
    (tr  'class "manual-section" 
      (td 'class "manual-section" 'colspan "4"  
         (if (empty-string? explicit-section-id) '() (a 'name explicit-section-id))
         (a 'name section-id)

         (section-navigation-banner next-section-number) (br)

         (span 'class "manual-section-number" (as-string number))
         (span 'class "manual-section-title" title-string)))))

(define (plain-manual-section-title-level-2 title-string)
    (tr  'class "manual-section" 
      (td 'class "manual-section-level-2" 'colspan "4"  
         (span 'class "manual-section-title-level-2" title-string))))



(define (plain-manual-section-body body)
  (tr (td 'colspan "4" 'class "manual-section-description" body)))

; This abstraction returns the empty contribution, and it has a side effect
; on the variable explicit-section-id
(define (plain-manual-section-id id)
  (set! explicit-section-id (as-string id))
  explicit-space)


(define (plain-manual-xml-in-laml-example-form f)
 (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Example form"
    (manual-plain-css-class "form")
      (cond ((string? f) f)
            (else (as-quoted-string-1 f)))))

(define (plain-manual-content-model cm)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "XML content model"
    (manual-plain-css-class "xml-content-model") cm))

(define (plain-manual-attributes . attr-list)
  (if (not (null? attr-list))
     (let* ((sorted-attr-list
               (sort-list attr-list (lambda (x y) (string<=? (second x) (second y)))))
            (attr-list-1 (if (eq? attribute-sorting-kind 'by-attribute-name) sorted-attr-list attr-list))
            (first-attr (car attr-list-1))
            (rest-attrs (cdr attr-list-1)))
       (cons
         (single-attribute-presentation 
            (manual-attribute-name first-attr) (manual-attribute-type first-attr)
            (manual-attribute-presence first-attr) (manual-attribute-description first-attr) (length attr-list-1) )
                                        
         (map 
           (lambda (attr)
             (single-attribute-presentation (manual-attribute-name attr) (manual-attribute-type attr)
                                            (manual-attribute-presence attr) (manual-attribute-description attr) #f))
           rest-attrs)))
     '())
)

(define manual-attribute-name (make-selector-function 2 "manual-attribute-name"))
(define manual-attribute-type (make-selector-function 3 "manual-attribute-type"))
(define manual-attribute-presence (make-selector-function 4 "manual-attribute-presence"))
(define manual-attribute-description (make-selector-function 5 "manual-attribute-description"))

(define (single-attribute-presentation attr-name attr-type attr-presence-default attribute-explanation first-row)  
 (let* (
        (default-marking (lambda (x) (font-color red x)))
        (attribute-type-present 
          (lambda (at)
           (cond ((string? at) 
                    (con
                      at
                      (if (member attr-presence-default (list "#REQUIRED" "#IMPLIED"))
                          ""
                          (con (horizontal-space 5) "(" (font-color red attr-presence-default) ")"))))
                 ((list? at) (con "(" (separate-list (mark-default at attr-presence-default default-marking) " | ") ")"))
                 (else "?????"))))
        (required-attribute? (lambda (ap) (equal? "#REQUIRED" (upcase-string attr-presence-default))))
        (presence-indication 
         (if (required-attribute? attr-presence-default) (font-color red (b " *")) ""))
       )
  (if first-row 
      (manual-slice-4-rowspan first-row
              (manual-plain-css-class "entry-kind") "XML attributes"
              (manual-plain-css-class "attribute-name") (con attr-name presence-indication)
              (manual-plain-css-class "attribute-type") (attribute-type-present attr-type)
              (manual-plain-css-class "attribute-description") attribute-explanation
              (con "Required: " (font-color red (b "*")) (br)
                 "Default values: " (font-color red "red") )
           )
      (manual-slice-3
              (manual-plain-css-class "attribute-name") (con attr-name presence-indication)
              (manual-plain-css-class "attribute-type") (attribute-type-present attr-type)
              (manual-plain-css-class "attribute-description") attribute-explanation
           )
  )))


; Make val in lst with the mark m. Compare elements with equal?
(define (mark-default lst val m)
 (if (not (or (equal? val "#REQUIRED") (equal? val "#IMPLIED")))
     (map (lambda (el) (if (equal? val el) (m el) el)) lst)
     lst))


  

 


; ---------------------------------------------------------------------------------------------------
; HEADER PLAIN STYLE:


(define (header-manual-title title-string)
  (td 'class (manual-header-css-class "title") (a-tag (string-append "#" title-string) title-string)))

(define (header-manual-form form-list)
  (td 'class (manual-header-css-class "form") (as-string form-list)))

(define (header-manual-description descr)
  (td 'class (manual-header-css-class "description") (strip-initial-spaces (manual-first-sentence-in-string (xml-render-as-xml descr))))) 

(define (header-manual-library library)
 (b library))

; ---------------------------------------------------------------------------------------------------
; TABLE OF CONTENTS STYLE:

(define (toc-manual-section-title title-string)
  title-string
 ; (a-tag (string-append "#" title-string) title-string)
 )


;; Return the saved name.manlsp manual info. Skip manual meta information. Augment each entry with the library name and path information.
;; In that way we know for each manual entry where it comes from.
;; The second parameter is optional. If provided it gives the directory from which to read the manlsp file.
;; The directory information is relative to the-library directory.
;; If it is not provided, read from the current directory.
(define (read-and-augment-lsp-file name . dir)
  (let ((dir-1 (if (not (null? dir)) (first dir) "")))
   (let ((res (cdr (file-read (string-append the-library dir-1 name ".manlsp")))))   ; cdr: Skip manual meta information.
    (map 
      (lambda (entry) (cons (list 'path dir-1) (cons (list 'library name) entry)))
      res))))



; ;; The column widths of the master index. A list of 4 integers.
; (define master-manual-index-width-list (list 140 70 300 400))
; 
; ;; Return the master index given an augmented manual.
; ;; The augmentation is information about the library of each entry.
; (define (manual-masterindex augmented-manual style)
;   (table-1 1 master-manual-index-width-list (list white white white white)   
;     (map
;       masterindex-present-special
;       (sort-list 
;          (filter manual-page? augmented-manual)  ; disregard manual sections for the index
;           manual-index-order-leq?))))
; 
; (define (masterindex-present-special entry)
;   ; We use this special presentation because we need a non-local reference from the first column.
;   ; Thus we do not use the nice style-based presentation, already prepared for the library entry
;   ; this information cannot be made from the title alone.
;   (let ((title (cadr (assq 'title entry)))
;         (library (cadr (assq 'library entry)))
;         (path (cadr (assq 'path entry)))
;         (form (form-of-entry entry))
;         (description (cadr (assq 'description entry))))
;     (list (master-index-ref title library path) (b library) (as-string form) (manual-first-sentence-in-string description))))
; 
; ; Return the form or xml-in-laml-example form of entry
; (define (form-of-entry entry)
;  (let ((form (get-defaulted-prop 'form entry #f))   ; TOO FEW PARAMETERS!
;        (xml-in-laml-ex-form (get-defaulted-prop 'xml-in-laml-example-form entry #f))
;       )
;   (cond (form form)
;         (xml-in-laml-ex-form xml-in-laml-ex-form)
;         (else "??"))))  
; 
; (define (master-index-ref title library path)
;   (a-tag (string-append path library ".html" "#" title) title))
; 
; 
; ;; NOT USED.
; ;; Write a master index to the file manual-name.html.
; ;; The second parameter is optional. If provided it gives the directory of the resulting file. If not, write in the current directory.
; ;; A master index is a normal index, but in addition it contains a libray column
; ;; Normally the parameters all-entries are made by mapping read-and-augment-lsp-file over the saved lsp files with
; ;; manual information. Next these lists are append accumulated.
; (define (make-master-index all-entries . dir)
;   (let ((dir-1 (if (not (null? dir)) (first dir) ""))
;         (yr (car (time-decode (current-time))))
;        )
;    (write-html '(raw prolog)
;     (html 
;       (head (html:title the-manual-title))
;       (body
;         (left-middle-right-banner (when-generated) (span "Copyright " copyright (as-string yr) "," _ "Kurt Nørmark") (laml-home-button 0 "laml-home.gif"))
;         (h 1 the-manual-title) (p) 
;         (present-author-info the-manual-author) (p)
;         (if (and #f home-url) (con (a-tag home-url "Home") (p)) "")
;         (indent-pixels 12 
;            (con (em "Alphabetic index:") 
;            (manual-masterindex all-entries 'header-manual-style))) (p)
;         (font-1 2 red (when-generated)) (br)
;         (font-1 2 red end-remark)
;        ))
;      (string-append dir-1 (as-string manual-name) ".html"))))


(define (manual-table-of-contents manual style)
 (let ((sections (filter manual-section? (filter page-to-be-presented? manual)))
       (toc-width (accumulate-right + 0 manual-index-width-list)))
  (n-column-list 3
    (map2
      (lambda (manual-section section-number) 
        (let* ((formatted-section (format-manual-page manual-section style style))
               (filtered-section-text (filter (compose not white-space-related?) formatted-section))
               (section-text (car filtered-section-text))
              )
          (a-tag
            (string-append "#" (section-id section-number))
            (font-size 2 (con (as-string section-number) _ ". " section-text))))
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
  (make-section-id next-section-number))

; The numerical section id of section number
(define (make-section-id number)
 (string-append "SECTION" (as-string number)))
  

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

; Global variables, assigned by manual-from-parsed-dtd

; The list of parsed elements, in the same order as parsed-dtd-attributes
(define parsed-dtd-elements #f)

; The list of parsed attributes, in the same order as parsed-dtd-elements
(define parsed-dtd-attributes #f)

;; Return a manual list, as extracted from a parsed dtd.
;; .parameter dtd-list-structure the Lisp structure, as generated by the LAML DTD parser.
;; .parameter mirror-name-prefix the prefix of all mirror functions of the XML language. A string. Use the empty string if no explicit name-prefix is used.
(define (manual-from-parsed-dtd dtd-list-structure mirror-name-prefix)
 (let* ((scheme-names (map symbol-of-scheme-knowledge (read-scheme-knowledge 5)))
        (elements   (filter (lambda (x) (eq? 'element (tag-dtd x))) dtd-list-structure))
        (attributes (filter (lambda (x) (eq? 'attribute (tag-dtd x))) dtd-list-structure))

        (filtered-elements (filter (negate (scheme-name-element scheme-names)) elements)) 
        (filtered-attributes (filter (negate (scheme-name-attribute scheme-names)) attributes))

        (sorted-filtered-attributes              ; attributes sorted in the same order as elements
         (sort-list
          filtered-attributes
          (generate-leq (map (compose as-symbol cadr) filtered-elements) (compose as-symbol cadr)))))

  ; Assign the global variables parsed-dtd-elements and parsed-dtd-attributes
  (set! parsed-dtd-elements filtered-elements)
  (set! parsed-dtd-attributes sorted-filtered-attributes)

  ; do the real work..
  (map2 (lambda (el attr) (manual-entry-from-parsed-dtd el attr mirror-name-prefix)) 
        filtered-elements sorted-filtered-attributes)))


(define (manual-entry-from-parsed-dtd el attr mirror-name-prefix)
 (let* ((title-contr (string-append mirror-name-prefix (element-name-dtd el)))
        (content-contr (element-content-model-dtd el))
        (attr-list-contr (attribute-list-dtd attr))
        (attribute-maker (lambda (alc) (cons 'attribute (eliminate-fixed-component alc))))
        (cross-ref-elements (all-elements-using (element-name-dtd el) parsed-dtd-elements))
        (cross-ref-length (length cross-ref-elements))
       )
   (filter (compose not null?)
    (list
     (list 'kind "manual-page")
     (list 'title title-contr)
     (list 'content-model (present-content-model content-contr))
     (cons 'attributes (map attribute-maker attr-list-contr))
     (if (> cross-ref-length 0) 
      (list 'cross-references 
            (cond ((> cross-ref-length 1)
                   (cons 'internal-references 
                         (cons "enclosing elements" cross-ref-elements)))
                  ((= cross-ref-length 1)
                   (cons 'internal-references 
                         (cons "enclosing element" cross-ref-elements)))
                  (else (cons 'internal-references '()))))
      '())
   ))
 )
)

; Return a predicate that checks if an element corresponds to a Scheme name.
(define (scheme-name-element scheme-name-list)
  (lambda (element)
   (turn-into-boolean (memq (as-symbol (element-name-of element)) scheme-name-list))))

; Return a predicate that checks if an attribute corresponds to a Scheme name.
(define (scheme-name-attribute scheme-name-list)
  (lambda (element)
   (turn-into-boolean (memq (as-symbol (attribute-name-of element)) scheme-name-list))))



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
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'sequence-with-optionals))
;        (string-append 
;         "(" 
;         (list-to-string (map (lambda (x) (if (symbol? x) (string-append (mr (as-string x)) "?") (mr x))) (cdr content-contr)) ",")
;         ")" ))
; 
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'one-or-more))
;        (string-append 
;         "(" 
;         (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
;         ")+"))
; 
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'zero-or-more))
;        (string-append 
;         "(" 
;         (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
;         ")*"))
; 
;       ((and (symbol? content-contr) (eq? 'pcdata-checker content-contr)) "(#PCDATA)")
; 
;       ((symbol? content-contr) (as-string content-contr))
; 
;       (else (laml-error "present-content-model: unknown kind of content model:" content-contr)))))

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
            (con
             "(#PCDATA |"
             (separate-list (map mr (sorter choice-list)) " | ")
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
              (con (mr (as-string name)) (mult-unparse mult))))
           ((eq? kind 'seq)
            (con "(" (separate-list (map present-element-content body-list) ",") ")" (mult-unparse mult)))
           ((eq? kind 'choice)
            (con "(" (separate-list (map present-element-content body-list) " | ") ")" (mult-unparse mult)))
           ((eq? kind 'empty)        ; does not appear in parsed forms
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


; ---------------------------------------------------------------------------------------------------


(define html:title (xhtml10-transitional 'title))
(define html:table (xhtml10-transitional 'table))

(define con-space con)
(define con-par con)

(define (separate-list lst separator)
 (let ((lgt-lst (length lst)))
  (merge-lists-simple lst (make-list (- lgt-lst 1) separator))))



; ---------------------------------------------------------------------------------------------------

; Return a horizontal slize of a manual contribution with one cell.
(define (manual-slice-1 class contribution)
  (tr (td 'colspan "4" 'class class contribution)))

; Return a horizontal slize of a manual contribution with two cells.
; As an optional parameter this function accept an explanation of the left contribution.
(define (manual-slice-2 left-class left-contribution right-class right-contribution . optional-parameter-list)
 (let ((explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 
        left-contribution 
        (if explanation
            (list (br) (span 'class "manual-kind-explanation" explanation))
            '()))
    (td 'colspan "3" 'class right-class right-contribution))))

; Return a horizontal slize of a manual contribution with three cells.
; As an optional parameter this function accept an explanation of the attribute
(define (manual-slice-3 left-class left-contribution middle-class middle-contribution right-class right-contribution . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 
        left-contribution 
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class middle-class middle-contribution)
    (td  'class right-class right-contribution))))

(define (manual-slice-3-rowspan rowspan left-class left-contribution middle-class middle-contribution right-class right-contribution . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 'rowspan (as-string rowspan)
        left-contribution 
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class middle-class middle-contribution)
    (td  'class right-class 'colspan "2" right-contribution))))


(define (manual-slice-4-rowspan rowspan class-1 contribution-1 class-2 contribution-2 class-3 contribution-3 class-4 contribution-4  . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class class-1 'rowspan (as-string rowspan)
        contribution-1
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class class-2 contribution-2)
    (td  'class class-3 contribution-3)
    (td  'class class-4 contribution-4))))

(define (manual-plain-css-class class-name-suffix)
  (string-append "manual-plain" "-" class-name-suffix))

(define (manual-header-css-class class-name-suffix)
  (string-append "manual-header" "-" class-name-suffix))

; ---------------------------------------------------------------------------------------------------------------

; CSS stylesheet copying for LAML scripts that allows easy global change of stylesheet in the LAML distribution.
; NOT USED.
(define (do-copy-css-stylesheet! manual-software-css-pre-filepath manual-software-css-filepath manual-target-css-filepath)
    (write-text-file
     (string-append
      (read-text-file-if-exists manual-software-css-pre-filepath) CR CR
      (read-text-file-if-exists manual-software-css-filepath)
      )
     manual-target-css-filepath
     ))

; ---------------------------------------------------------------------------------------------------------------

; Check that all elements from dtd-page-list is contained in authored-page-section-list.
; Also check that all described elements in authored-page-section-list are in the DTD.
(define (merged-xml-dtd-completeness-control! authored-page-section-list dtd-page-list)
  (let* ((authored-page-list (filter manual-page? authored-page-section-list))
         (authored-element-names (map (lambda (page-str) (car (get 'title page-str))) authored-page-list))
         (dtd-element-names (map (lambda (page-str) (car (get 'title page-str))) dtd-page-list))
         (non-authored-elements (list-difference dtd-element-names authored-element-names string=?))
         (non-dtd-elements      (list-difference authored-element-names dtd-element-names string=?))
        )
    (if (not (null? non-authored-elements))
        (display-warning "The following XML DTD elements are not described:" (list-to-string non-authored-elements ", ")))

    (if (not (null? non-dtd-elements))
        (display-warning "The following documented elements are not in the XML DTD:" (list-to-string non-dtd-elements ", ")))
  )
)

; ---------------------------------------------------------------------------------------------------
; Manual section navigation

(define (section-navigation-banner section-number)
 (let ((title-of-prev "Previous manual section")
       (title-of-next "Next manual section")
       (title-of-up "Manual top")
       (up #t) 
       (prev (> section-number 1))
       (next (< section-number max-section-number))
      )
  (span
    (if up (a 'href "#MANUAL-TOP" (image "small-up.gif" title-of-up)) (image "small-up-blind.gif" "")) 
    
    (if prev 
        (a 'href  (string-append "#" (make-section-id (- section-number 1))) (image "small-prev.gif" title-of-prev))
        (image "small-prev-blind.gif" ""))
 
    (if next
        (a 'href  (string-append "#" (make-section-id (+ section-number 1))) (image "small-next.gif" title-of-next))
        (image "small-next-blind.gif" "")))))

(define (image file-name help-text) (img 'src (string-append (man-image-prefix) file-name) 'title help-text 'alt "" 'border "0"))

; The relative path from the manual directory to the image directory 
; Inside the LAML software packages, manual images are in the images directory of the LAML root directory.
; Manuals outside the LAML directory will hold a man-images subdirectory with a copy of the images.
(define (man-image-prefix)
  (if laml-manual-stuff
      (string-append (laml-dir-prefix the-manual-destination-path) "images/")  ; earlier:  (laml-home-url-prefix 0)
      "man-images/"))

