; The new LENO surface featuring an XML-like LAML interface.
; Responsible for the transition from XML-in-LAML conventions to the original conventions.
; It loads lecture-notes-kernel.scm which contains the bulk amount of LENO software.

; -----------------------------------------------------------------------------
; Loading of libaries:

; general.scm is loaded by laml.scm
(lib-load "cgi.scm")
(lib-load "encode-decode.scm")


(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")



; -----------------------------------------------------------------------------------------------------------

(define default-importance 0)

(define new-note-page-list '())

; The version of note-page which pertains to the XML-in-LAML interface
; Collects as a side effect a list of elements in the variable new-note-page-list
(define (do-note-page element-ast)    ; (define (do-note-page . x)
 (let ((note-page-id (as-symbol (get 'id (new-element-attributes element-ast)))) 
       (note-page-importance (as-number (defaulted-get 'importance (new-element-attributes element-ast) default-importance))))
   (set! page-id note-page-id)
   (let* ((new-style-elements (filter (compose not forced-white-space?) (new-element-contents element-ast)))
          (new-style-attributes (new-element-attributes element-ast))
          (old-style-elements (map new-style-element-to-old-style-element new-style-elements))

          (drop? (as-boolean (defaulted-get 'drop new-style-attributes #f)))
          )
     (if (not drop?)
         (begin (set! new-note-page-list (cons element-ast new-note-page-list))
                (apply original-note-page (cons note-page-importance (cons note-page-id old-style-elements))))
         (begin

           (let ((lsp-page (make-lsp-page note-page-id old-style-elements))) ; add dropped note-page to internal lsp file. Experimental as august 21, 2007
             (set! lsp-contents-in-this-lecture (cons lsp-page lsp-contents-in-this-lecture)))

           (if (>= lecture-note-verbose-mode 1)
               (begin
                 (display-message (string-append (as-string lecture-id) ":" (as-string note-page-id) "  " "DROPPED"))
                 )))))))


; -----------------------------------------------------------------------------------------------------------

; corresponds to make-element in old lecture-notes.scm
(define (make-old-element tag id contents attributes)
 (let ((the-id (if (boolean? id) id (as-symbol id))))
  (list tag the-id contents attributes)))


; ---------------------------------------------------------------------------------------------------
; New to old element transformation.

(define (new-style-element-to-old-style-element new-style-element)
 (let* ((tag (new-element-tag new-style-element))
        (attribute-list (new-element-attributes new-style-element))
        (transformer (find-new-to-old-transformer tag))

        (drop-element? (as-boolean (defaulted-get 'drop attribute-list #f)))
       )
   (if (>= lecture-note-verbose-mode 2) 
       (display-message (string-append "  " (as-string tag) (if drop-element? " DROPPED" ""))))
   (transformer new-style-element)))

(define (find-new-to-old-transformer tag)
  (cond ((eq? tag 'title) title-new-to-old-transformer)
	((eq? tag 'text) text-new-to-old-transformer)
	((eq? tag 'items) items-new-to-old-transformer)
	((eq? tag 'item) item-new-to-old-transformer)
	((eq? tag 'point) point-new-to-old-transformer)
	((eq? tag 'program) program-new-to-old-transformer)  ; dropped - not used.
	((eq? tag 'source-program) source-program-new-to-old-transformer)
	((eq? tag 'color-specification) color-specification-new-to-old-transformer)
	((eq? tag 'image) image-new-to-old-transformer)
	((eq? tag 'concepts) concepts-new-to-old-transformer)
	((eq? tag 'example) example-new-to-old-transformer)
	((eq? tag 'opposing) opposing-new-to-old-transformer)
	((eq? tag 'comment) comment-new-to-old-transformer)
	((eq? tag 'language-syntax) syntax-new-to-old-transformer)
	((eq? tag 'slide-space) slide-space-new-to-old-transformer)
	((eq? tag 'tabular) tabular-new-to-old-transformer)
	((eq? tag 'note-text) note-text-new-to-old-transformer)
	((eq? tag 'slide-text) slide-text-new-to-old-transformer)
	((eq? tag 'cross-references) cross-references-new-to-old-transformer)
	((eq? tag 'informal-reference) informal-reference-new-to-old-transformer)
	((eq? tag 'internet-reference) internet-reference-new-to-old-transformer)
	((eq? tag 'note-reference) note-reference-new-to-old-transformer)
	((eq? tag 'bibtex-reference) bibtex-reference-new-to-old-transformer)
	((eq? tag 'exercise) exercise-new-to-old-transformer)
	((eq? tag 'formulation) formulation-new-to-old-transformer)
	((eq? tag 'solution) solution-new-to-old-transformer)
	((eq? tag 'index-words) index-words-new-to-old-transformer)
	((eq? tag 'image-series) image-series-new-to-old-transformer)
	((eq? tag 'section-title) section-title-new-to-old-transformer)
	((eq? tag 'slide-image) slide-image-new-to-old-transformer)
	((eq? tag 'concept-list) concept-list-new-to-old-transformer)
	((eq? tag 'concept) concept-new-to-old-transformer)
	((eq? tag 'applet-program) applet-program-new-to-old-transformer)
	((eq? tag 'applet-param) applet-param-new-to-old-transformer)
	((eq? tag 'synopsis) synopsis-new-to-old-transformer)
	((eq? tag 'synopsis-item) synopsis-item-new-to-old-transformer)
	((eq? tag 'quotation) quotation-new-to-old-transformer)
	((eq? tag 'quiz) quiz-new-to-old-transformer)
	((eq? tag 'question) question-new-to-old-transformer)
	((eq? tag 'answers) answers-new-to-old-transformer)
	((eq? tag 'answer) answer-new-to-old-transformer)
	((eq? tag 'show-and-speak) show-and-speak-new-to-old-transformer)
	((eq? tag 'slide-part) slide-part-new-to-old-transformer)
	((eq? tag 'program-part) program-part-new-to-old-transformer)
	((eq? tag 'image-series-part) image-series-part-new-to-old-transformer)
;	((eq? tag 'exercise-part) exercise-part-new-to-old-transformer)
	((eq? tag 'lecturer-photos) lecturer-photos-new-to-old-transformer)
	((eq? tag 'lecturer-photos-and-logo) lecturer-photos-and-logo-new-to-old-transformer) 
	((eq? tag 'elucidate) elucidate-new-to-old-transformer)
	((eq? tag 'splice-page-with) splice-page-with-new-to-old-transformer)
	((eq? tag 'splice-page-without) splice-page-without-new-to-old-transformer)
	((eq? tag 'side-track) side-track-new-to-old-transformer)
	((eq? tag 'svg-image) svg-image-new-to-old-transformer)
	((eq? tag 'flash-image) flash-image-new-to-old-transformer)
	((eq? tag 'meta-text) meta-text-new-to-old-transformer)
	((eq? tag 'theme-text) theme-text-new-to-old-transformer)
	(else (error (string-append "Cannot find new to old transformer function: " (as-string tag)))))
)

; Return a sub-clause tagged clause-name from clause-list. Clause-list is
; assumed to be a list of subclauses (list of objects made by make-element). 
; If the sub-clause does not exists, return a default element clause with supplied contents from
; the optional parameter, of #f if no optional parameter is supplied.
(define (sub-clause clause-name clause-list . optional-parameter-list)
 (let ((default-content (optional-parameter 1 optional-parameter-list #f))
       (res (find-in-list
	     (lambda (clause)
	       (and (element-structure? clause) (eq? (new-element-tag clause) clause-name)))
	     clause-list))
       )
   (cond (res res)
         (default-content (make-element-structure (as-symbol clause-name) (list default-content) '()))
         (else #f))))

; Return the list of subclauses tagged with clause-name.
(define (all-sub-clauses clause-name clause-list)
 (filter
   (lambda (element)
     (eq? (new-element-tag element) clause-name))
   clause-list))




; --------------------------------------------------------------------------------------------------------------------------------
; HTML rendering of HTML elements nested in the the LENO XML elements. ONLY!
; Simple recursive processing.

(define (html-render ast)
 (let* ((tag-name (as-string (new-element-tag ast))) 
        (contents-list (new-element-contents ast))
        (linearized-contents (html-linearize-contents contents-list))
        (attribute-properlist (alist-to-propertylist (new-element-attributes ast)))
        (kind (if (null? contents-list) 'single 'double))
       )
  (if (eq? kind 'double)
      (itag-html tag-name linearized-contents attribute-properlist)
      (itag1-html tag-name attribute-properlist))))

(define (html-linearize-contents contents-list)
  (html-linearize-contents-1 contents-list ""))

(define (html-linearize-contents-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list))
            (html-linearize-contents-1 (cdr contents-list) (string-append res (html-text-transform (car contents-list)))))
        ((forced-white-space? (car contents-list)) (html-linearize-contents-1 (cdr contents-list) (string-append res " ")))
        ((element-structure? (car contents-list)) 
           (html-linearize-contents-1 (cdr contents-list) (string-append res (html-render (car contents-list)))))))


; Return a balanced html tag of both start and end tag, based on the parameters name (tag name) contents and attributes.
; The attributes is supposed to be a property list of symbol string pairs.
; The 'i' in the function name means 'internal'.
(define (itag-html name contents attributes)
 (if (null? attributes)
  (string-append "<" (as-string name) ">"
                  (as-string contents)
                  "</" name ">")
  (let* ((attributes (linearize-attributes attributes))
        (html-attributes (car attributes))
         (css-attributes (cdr attributes)))
   (string-append "<" (as-string name) " " html-attributes
                  (if (empty-string? css-attributes) "" (string-append " style = " (string-it css-attributes)))
                   ">"
                  (as-string contents)
                  "</" (as-string name) ">"))))
; Return a single tag of name with attributes.
; The attributes is supposed to be a property list of symbol string pairs.
; The 'i' in the function name means 'internal'.
(define (itag1-html name attributes)
 (if (null? attributes)
	 (string-append "<" (as-string name) (if xml-syntax? "/>" ">"))
	 (let* ((attributes (linearize-attributes attributes))
		(html-attributes (car attributes))
		(css-attributes (cdr attributes)))
	   (string-append "<" (as-string name) " " html-attributes 
			  (if (empty-string? css-attributes) "" (string-append " style = " (string-it css-attributes)))
			  (if xml-syntax? "/>" ">")
			  ))))

; Transform each character in the string str, using the HTML char transformation table, html-char-transformation-table.
; Very inefficient in memory usage - therefore eliminated.

(define (html-text-transform str)
  (html-text-transform-1 str (string-length str) 0 '())
)
 
 
(define (html-text-transform-1 str str-lgt i res)
  (cond ((= i str-lgt) (list-to-string (reverse res) ""))
        (else (html-text-transform-1 str str-lgt (+ i 1) (cons (html-char-transform (string-ref str i)) res)))))


; END HTML rendering of HTML elements nested in the the LENO XML elements. ONLY!


; ------------------------------------------------------------------------------------------------------------------------


; ELEMENT TRANSFORMERS:

; Generic transformers:

(define (single-constituent-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (textual-contents (aggregated-contents e-contents))
       )
  (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (list textual-contents)
     e-attributes)))

; A generic transformer for main-text and annotation clauses (text, point, etc). 
; I.e., transformers with two constituents: main-text and annotation.
(define (main-text-annotation-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))
        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list main ann)
     e-attributes)))


(define title-new-to-old-transformer main-text-annotation-transformer)

(define text-new-to-old-transformer main-text-annotation-transformer)

(define (section-title-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     e-contents
     e-attributes)))

(define (items-new-to-old-transformer new-element-structure)  ; plural
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure))) ; list of item clauses
        (e-attributes (new-element-attributes new-element-structure))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map item-new-to-old-transformer e-contents) ; singular
     e-attributes)))

(define (item-new-to-old-transformer new-element-structure)  ; singular
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))
        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
        (sub-items-clause (sub-clause 'items e-contents)) ; an items clause
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (if sub-items-clause
         (list main ann (items-new-to-old-transformer sub-items-clause))
         (list main ann))
     e-attributes)))

(define point-new-to-old-transformer main-text-annotation-transformer)

(define (source-program-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (src-attr (get 'src e-attributes))
        (from-mark-attr (defaulted-get 'from-mark e-attributes #f))
        (to-mark-attr (defaulted-get 'to-mark e-attributes #f))
        (slide-mode-attr (defaulted-get 'slide-mode e-attributes "inline"))
        (book-mode-attr (defaulted-get 'book-mode e-attributes "inline"))
        (background-color-attr (defaulted-get 'background-color e-attributes #f))
        (indexed-attr (as-boolean (defaulted-get 'indexed e-attributes #t)))
        (index-title (defaulted-get 'index-title e-attributes ""))
        (kind (defaulted-get 'kind e-attributes "source-program"))   ; intended to separate real source programs and listings
        (program-annotations (as-boolean (defaulted-get 'program-annotations e-attributes "true")))  ; normally true
                                                                                                     ; can be use to suppress
                                                                                                     ; annotations in note view.
        (color-decoration-list (sub-clause 'color-decorations e-contents))
        (color-spec-list (if color-decoration-list 
                             (filter (compose not forced-white-space?) (new-element-contents color-decoration-list))
                             '()))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
        (end-char-of  ; return last non-blank char, or #f if blank string
         (lambda (str)
           (if (not (blank-string? str))
	       (let ((strip-str (strip-trailing-characters white-space-char-list str)))
		 (string-ref strip-str (- (string-length strip-str) 1)))
	       #f)))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       src-attr
       (if (and from-mark-attr to-mark-attr) (list from-mark-attr to-mark-attr) "")
       (map color-specification-new-to-old-transformer color-spec-list)
       (list 
         (if (equal? slide-mode-attr "inline") 'slide-inline 'slide-external)
         (if (equal? book-mode-attr "inline") 'book-inline 'book-external)
       )
       (string-append 
         main  (if (eqv? #\. (end-char-of main)) " " ". ")  ; ensure trailing point more carefully
         ann)
       background-color-attr
       indexed-attr
       index-title
       kind
       program-annotations
     )
     e-attributes)))

(define (color-specification-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure)) 

        (from-mark-attr (get 'from-mark e-attributes))
        (to-mark-attr (get 'to-mark e-attributes))
        (color-attr (get 'color e-attributes))
        (face-attr (defaulted-get 'face e-attributes "bold"))
        (repetition-attr (defaulted-get 'repetition e-attributes "1"))
       )
   (list 
     from-mark-attr
     to-mark-attr
     color-attr
     (as-symbol face-attr)
     (as-number repetition-attr)
   )
 )
)

(define (image-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))

        (src-attr (get 'src e-attributes))
        (alignment-attr (defaulted-get 'alignment e-attributes 'vertical))
        (first-attr (defaulted-get 'first e-attributes 'picture))
        (second-attr (defaulted-get 'second e-attributes #f))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       src-attr
       main
       (map as-symbol
            (if second-attr (list alignment-attr first-attr second-attr) (list alignment-attr first-attr)))
      )
     e-attributes)))

(define example-new-to-old-transformer main-text-annotation-transformer)

(define (opposing-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map opposing-item-new-to-old-transformer e-contents)
     e-attributes)))

(define (opposing-item-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (left-contents  (aggregated-contents (new-element-contents (sub-clause 'left-item e-contents))))
        (right-contents (aggregated-contents (new-element-contents (sub-clause 'right-item e-contents))))
       )
  (make-old-element 
     'item   ; !!
     (defaulted-get 'id e-attributes #f)
     (list left-contents right-contents)
     e-attributes)))

(define comment-new-to-old-transformer single-constituent-transformer)

(define syntax-new-to-old-transformer main-text-annotation-transformer)

(define (slide-space-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))
       )
   (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (list )
     e-attributes)))

(define gmap (curry-generalized map))

(define (tabular-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (border-attribute (defaulted-get 'border e-attributes "1"))

        (annotation-text  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
        (row-width-list (sub-clause 'row-widths e-contents))
        (row-list (all-sub-clauses 'row e-contents))
       )
   (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (list 
       (as-number border-attribute)
       (map (compose as-number tabular-cell-extractor) (filter (compose not forced-white-space?) (new-element-contents row-width-list)))
       (map 
          (gmap tabular-cell-extractor)  ; a tabular-cell-extractor mapper
          (map
            (lambda (x) (filter (compose not forced-white-space?) x))
           (map 
            new-element-contents
            row-list))  ; list of list of cell elements
       )
       annotation-text
     )
     e-attributes)))

; Given a cell element structure, extract the cell contentst string
(define (tabular-cell-extractor cell-element-structure)
  (aggregated-contents (new-element-contents cell-element-structure)))

(define note-text-new-to-old-transformer single-constituent-transformer)

(define slide-text-new-to-old-transformer single-constituent-transformer)

(define (cross-references-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))
       )
   (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (list  ; the old cross-reference surface function takes a single list of cross references
       (map new-style-element-to-old-style-element e-contents)
     )
     e-attributes)))

(define (internet-reference-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (href-attr (get 'href e-attributes))
        (anchor-text-contents (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (location-hints-contents (sub-clause 'location-hints e-contents #f))
        (make-old-internet-reference-element 
          (lambda (anchor url loc-hints) (list 'internet-reference anchor url loc-hints)))
       )
   (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list anchor-text-contents href-attr (location-hints-transforation location-hints-contents))
     e-attributes)
;    (make-old-internet-reference-element
;       anchor-text-contents 
;       href-attr 
;       (cons 'internet-reference (location-hints-transforation location-hints-contents)))
   ))

(define (note-reference-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (lecture-id-attr (get 'lecture-id e-attributes))
        (page-id-attr (get 'page-id e-attributes))
        (anchor-text-contents (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (location-hints-contents (sub-clause 'location-hints e-contents #f))
        (make-old-note-reference-element 
          (lambda (anchor lecture-id page-id loc-hints) (list 'note-reference anchor lecture-id page-id loc-hints)))
       )
    (make-old-element 
      e-tag
      (defaulted-get 'id e-attributes #f)
      (list anchor-text-contents (as-symbol lecture-id-attr) (as-symbol page-id-attr) (location-hints-transforation location-hints-contents))
      e-attributes)
))

(define (informal-reference-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (title-id-attr (get 'title e-attributes))
        (anchor-text-contents (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (location-hints-contents (sub-clause 'location-hints e-contents))
        (make-old-informal-reference-element 
           (lambda (title text loc-hints) (list 'informal-reference title text loc-hints)))
       )
    (make-old-element 
      e-tag
      (defaulted-get 'id e-attributes #f)
      (list title-id-attr anchor-text-contents (location-hints-transforation location-hints-contents))
      e-attributes)
))

(define (bibtex-reference-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (key-attr (get 'key e-attributes))
        (id-attr (defaulted-get 'id e-attributes #f))
        (location-hints-contents (sub-clause 'location-hints e-contents))
        (make-old-informal-reference-element 
           (lambda (title text loc-hints) (list 'informal-reference title text loc-hints)))
       )
    (make-old-element 
      e-tag
      (if id-attr id-attr (downcase-string key-attr)) ; the downcased key attribute is used as  
      (list key-attr (location-hints-transforation location-hints-contents))
      e-attributes)
))



; bibtex-reference missing - also bibtex-files in leno preamble.


; Return a list of location hint symbols, or the empty list in case there is no location hints - 
; i.e., if location-hints-element is #f.
(define (location-hints-transforation location-hints-element)
 (if location-hints-element
     (let ((hint-elements (filter (compose not forced-white-space?) (new-element-contents location-hints-element))))
       (map (compose as-symbol (compose first element-contents)) hint-elements))
     '()))

(define (exercise-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (id-attr (get 'id e-attributes))
        (title-attr (get 'title e-attributes))
        (rank-attr (defaulted-get 'rank e-attributes #f))
        (formulation-element (sub-clause 'formulation e-contents))
        (solution-element (sub-clause 'solution e-contents))
       )
   (make-old-element 
     e-tag     
     id-attr
     (if solution-element
       (list (as-symbol id-attr) title-attr 
	     (formulation-new-to-old-transformer formulation-element) 
	     (solution-new-to-old-transformer solution-element)
             rank-attr
             )
       (list (as-symbol id-attr) title-attr 
	     (formulation-new-to-old-transformer formulation-element)
             #f
             rank-attr 
	     ))

     e-attributes)))

(define formulation-new-to-old-transformer single-constituent-transformer)
(define solution-new-to-old-transformer single-constituent-transformer)

(define (index-words-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (word-elements (all-sub-clauses 'index-word e-contents))
       )
  (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (map (compose aggregated-contents new-element-contents) word-elements)
     e-attributes)))

(define (image-series-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (image-series-items (all-sub-clauses 'image-series-item e-contents))

        (title-attr (get 'title e-attributes))
        (slide-mode-attr (defaulted-get 'slide-mode e-attributes "inline"))
        (book-mode-attr (defaulted-get 'book-mode e-attributes "inline"))
       )
  (make-old-element 
     e-tag     
     (defaulted-get 'id e-attributes #f)
     (list 
       title-attr
       (map image-series-item-new-to-old-transformer image-series-items)
       (list 
         (if (equal? slide-mode-attr "inline") 'slide-inline 'slide-external)
         (if (equal? book-mode-attr "inline") 'book-inline 'book-external)
       )
     )
     e-attributes)))

(define (image-series-item-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (src-attribute (get 'src e-attributes))

        (descr (aggregated-contents e-contents))
       )
   (list src-attribute descr) ; ! special purpose transformation
  ))

(define section-title-new-to-old-transformer single-constituent-transformer)

(define (slide-image-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (src-attr (get 'src e-attributes))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       src-attr
     )
     e-attributes)))

(define (concept-list-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (concept-element-list (all-sub-clauses 'concept e-contents))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map concept-new-to-old-transformer concept-element-list)
     e-attributes)))

(define (concept-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (concept-name-attr (get 'concept-name e-attributes))
        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       concept-name-attr main ann
     )
     e-attributes)))

(define (applet-program-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-contents-filtered (filter (compose not forced-white-space?) e-contents))
        (e-attributes (new-element-attributes new-element-structure))

        (code-attr (get 'code e-attributes))
        (code-base-attr (get 'code-base e-attributes))
        (height-attr (get 'height e-attributes))
        (width-attr (get 'width e-attributes))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (param-clauses (all-sub-clauses 'applet-param e-contents-filtered))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       code-attr code-base-attr main (as-number width-attr) (as-number height-attr)  
       (map new-style-element-to-old-style-element param-clauses)
     )
     e-attributes)))


(define (applet-param-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure)) ; a-list

        (name-attr (get 'name e-attributes))
        (value-attr (get 'value e-attributes))
       )
     (cons 
       (as-string name-attr)
       (as-string value-attr)
     )
 ))

(define (synopsis-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (synopsis-item-list (all-sub-clauses 'synopsis-item e-contents))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map synopsis-item-new-to-old-transformer synopsis-item-list)
     e-attributes)))

(define (synopsis-item-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list main ann)
     e-attributes)))

(define quotation-new-to-old-transformer main-text-annotation-transformer)

(define (quiz-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (question-part (sub-clause 'question e-contents))
        (answers-part  (sub-clause 'answers e-contents ""))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map new-style-element-to-old-style-element (list question-part answers-part))
     e-attributes)))

(define (answers-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (answer-element-list (all-sub-clauses 'answer e-contents))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map answer-new-to-old-transformer answer-element-list)
     e-attributes)))

(define (answer-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (correctness-attr (get 'correctness e-attributes))
 
        (answer-pos-element (sub-clause 'answer-possibility e-contents))
        (answer-clar-element (sub-clause 'answer-clarification e-contents))

       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       (aggregated-contents (new-element-contents answer-pos-element))
       (as-number correctness-attr)
       (aggregated-contents (new-element-contents answer-clar-element))
     )
     e-attributes)))

(define question-new-to-old-transformer single-constituent-transformer)

(define (show-and-speak-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (map new-style-element-to-old-style-element e-contents)
     e-attributes)))

(define (slide-part-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (number-attr (get 'number e-attributes))
        (seconds-attr (get 'seconds e-attributes))

       )
    (list 
       (as-symbol e-tag)
       (as-number number-attr)
       (as-number seconds-attr)
    )
  ))

(define (program-part-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (number-attr (get 'number e-attributes))
        (seconds-attr (get 'seconds e-attributes))

       )
    (list 
       (as-symbol e-tag)
       (as-number number-attr)
       (as-number seconds-attr)
    )
  ))

(define (image-series-part-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (number-attr (get 'number e-attributes))

        (image-part-list (all-sub-clauses 'image-part e-contents))
       )
    (list 
       (as-symbol e-tag)
       (as-number number-attr)
       (map image-part-new-to-old-transformer image-part-list)
    )
  ))

(define (image-part-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (seconds-attr (get 'seconds e-attributes))
       )
    (as-number seconds-attr)
  ))

(define (lecturer-photos-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (start-number-attr (get 'start-number e-attributes))
        (number-of-photos-attr (get 'number-of-photos e-attributes))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       (as-number start-number-attr)
       (as-number number-of-photos-attr)
     )
     e-attributes)
 )
)

(define (lecturer-photos-and-logo-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (start-number-attr (get 'start-number e-attributes))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       (as-number start-number-attr)
     )
     e-attributes)
 )
)

(define (elucidate-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (href-attr (get 'href e-attributes))
        (href-target (defaulted-get 'target e-attributes elucidator-default-target))  
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       (aggregated-contents e-contents)
       href-attr
       href-target
     )
     e-attributes)
 )
)

; form to transform:
;(splice-page-with 'lecture-id "lid" 'page-id "pid" 'element-id "i"
;    (leno-elements (element-name "n1") (element-name "n2") ... ))
(define (splice-page-with-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (lid (as-symbol (get 'lecture-id e-attributes)))
        (pid (as-symbol (get 'page-id e-attributes)))
        (el-id (defaulted-get 'element-id e-attributes #f))
        (el-id1 (if el-id (as-symbol el-id) el-id))

        (leno-elements-contents (new-element-contents (car e-contents)))
        (leno-elements-contents-1 (filter (compose not forced-white-space?) leno-elements-contents))
        (tag-symbol-list (map (compose as-symbol car new-element-contents) leno-elements-contents-1))

       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (if el-id1 
         (list tag-symbol-list lid pid el-id1)
         (list tag-symbol-list lid pid)
     )
     e-attributes)
 )
)

(define (splice-page-without-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (lid (as-symbol (get 'lecture-id e-attributes)))
        (pid (as-symbol (get 'page-id e-attributes)))
        (leno-elements-contents (new-element-contents (car e-contents)))
        (leno-elements-contents-1 (filter (compose not forced-white-space?) leno-elements-contents))
        (tag-symbol-list (map (compose as-symbol car new-element-contents) leno-elements-contents-1))

       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       tag-symbol-list lid pid
     )
     e-attributes)
 )
)


; form to transform:
; (side-track 
;   (main-text "side track text")
;   (annotation "side track explanation")
;   'track-lecture-id "lid")
(define (side-track-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (new-element-contents new-element-structure))
        (e-attributes (new-element-attributes new-element-structure))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (ann  (aggregated-contents (new-element-contents (sub-clause 'annotation e-contents ""))))

        (lid-0 (defaulted-get 'track-lecture-id e-attributes #f))   ; either lid or track-url must be given. 
        (lid (if (string? lid-0) (as-symbol lid-0) lid-0))

        (track-url (defaulted-get 'track-url e-attributes #f))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       main lid track-url ann
     )
     e-attributes)
 )
)


(define (svg-image-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))
        (inline-svg (sub-clause 'svg-inline e-contents))

        (src-attr (defaulted-get 'src e-attributes #f))
        (width-attr (get 'width e-attributes))
        (height-attr (get 'height e-attributes))
        (id (defaulted-get 'id e-attributes #f))
       )
  (make-old-element 
     e-tag
     id
     (list 
       src-attr width-attr height-attr
       main id inline-svg
      )
     e-attributes)))

(define (flash-image-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (main (aggregated-contents (new-element-contents (sub-clause 'main-text e-contents))))

        (src-attr (get 'src e-attributes))
        (width-attr (get 'width e-attributes))
        (height-attr (get 'height e-attributes))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       src-attr width-attr height-attr
       main
      )
     e-attributes)))


(define (meta-text-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (textual-contents (aggregated-contents e-contents))
        (type-attr (defaulted-get 'type e-attributes "normal"))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       textual-contents type-attr 
     )
     e-attributes)))

(define (theme-text-new-to-old-transformer new-element-structure)
 (let* ((e-tag (new-element-tag new-element-structure))
        (e-contents (filter (compose not forced-white-space?) (new-element-contents new-element-structure)))
        (e-attributes (new-element-attributes new-element-structure))

        (textual-contents (aggregated-contents e-contents))
       )
  (make-old-element 
     e-tag
     (defaulted-get 'id e-attributes #f)
     (list 
       textual-contents
     )
     e-attributes)))






; ---------------------------------------------------------------------------------------------------
; UTILITY STUFF TO PRODUCE AN XML SOURCE FILE FROM A LENO ELEMENT STRUCTURE.

; A utility function that converts an element structure to an AST ala the validating mirrors.
(define (element-structure-to-ast x)
 (letrec ((single-element? (lambda (x) (null? (new-element-contents x))))
          (make-ast (lambda (element-name contents attributes kind) 
                      (list 'ast (as-string element-name) contents attributes (as-symbol kind))))
         )
   (cond ((element-structure? x)
           (make-ast 
             (as-string (new-element-tag x))
             (map element-structure-to-ast (new-element-contents x))
             (alist-to-propertylist (new-element-attributes x))
             (if (single-element? x) 'single 'double)))
         ((forced-white-space? x) x)
         ((cdata? x) x)
         (else (error (string-append "element-structure-to-ast: Should not happen: " (as-string x)))))))

; Make a front matter element from the information in leno-front-matters-list,
; as captured by the function leno-front-matters.
(define (make-front-matters-element)
 (let* ((fml1
         (map (lambda (x) 
               (cond ((list? x) (make-element-structure (as-symbol (car x)) (list (cadr x)) '()))
                     (else x)))  leno-front-matters-list))
        (cont-attr (sort-element-parameters (cdr fml1) 'leno-front-matters))
       )
   (make-element-structure 
     'leno-front-matters
     (car cont-attr)
     (propertylist-to-alist (cdr cont-attr)))))


; Utility procedure. Call from an interactive laml prompt just after a LENO laml file has been processed.
; Write a pretty printed xml file from new-note-page-list on file-name, which is a file name
; with extension. The file is written in (startup-directory).
(define (make-xml-file-from-leno-elements file-name)
 (let* ((new-note-page-list-1 (reverse new-note-page-list))
        (make-ast (lambda (element-name contents attributes kind) 
		    (list 'ast (as-string element-name) contents attributes (as-symbol kind))))
        (ast-list (map element-structure-to-ast 
                       (cons leno-front-matters-element new-note-page-list-1)))
        (leno-ast (make-ast 'leno ast-list '() 'double))
       )
   (load (string-append laml-dir "tools/xml-html-support/xml-support.scm")) ; to use make-parse-tree and pretty-print-xml-parse-tree
;   (load (string-append laml-dir "styles/simple-xhtml1.0-strict-validating.scm")) ; to use ast-to-parse-tree
   (let* ((leno-parse-tree (ast-to-parse-tree leno-ast))
          (file-path (string-append (startup-directory) file-name))
         )
     (if (file-exists? file-path) (delete-file file-path))
     (write-text-file
      (pretty-print-xml-parse-tree leno-parse-tree)
      file-path))))


; ---------------------------------------------------------------------------------------------------

(load (leno-software-dir-file "lecture-notes-kernel.scm"))   

; Redefinition relative to lecture-notes-kernel.scm.
(define (file-and-directory-identification)
 (apply string-append
  (map format-key-value-pairs
   (list
     (list "LAML source file" (string-append note-source-directory lecture-id ))
     (list "Scheme system" scheme-system)
     (list "Scheme library directory" the-library)
     (list "Style Scheme file" (string-append leno-xml-in-laml-software-directory "lecture-notes.scm"))
   ))))
   